from fastapi import APIRouter, HTTPException, status, Depends, Request
from datetime import datetime, timezone
from motor.motor_asyncio import AsyncIOMotorDatabase

from models.user import (
    UserCreate, UserLogin, UserResponse, UserInDB, 
    PasswordReset, Token
)
from models.audit import UserActivity, UserSession
from utils.security import (
    verify_password, get_password_hash, generate_temp_password,
    create_access_token, get_current_user
)
from services.email_service import send_welcome_email

router = APIRouter(prefix="/auth", tags=["Authentication"])

# Database will be injected
db: AsyncIOMotorDatabase = None

def set_db(database: AsyncIOMotorDatabase):
    global db
    db = database


async def log_activity(user_id: str, user_email: str, user_name: str, action: str, resource: str, details: str = None):
    """Helper to log user activity (used for login/logout)"""
    # For login/logout, we don't need to log in activities since sessions track this
    # This function is kept for backward compatibility but can be skipped for auth actions
    pass


async def create_session(user_id: str, user_email: str, user_name: str):
    """Helper to create user session"""
    session = UserSession(
        user_id=user_id,
        user_email=user_email,
        user_name=user_name,
        login_time=datetime.utcnow().isoformat(),
        is_active=True
    )
    await db.user_sessions.insert_one(session.model_dump())


@router.post("/login", response_model=Token)
async def login(credentials: UserLogin):
    """User login with email and password"""
    user = await db.users.find_one({"email": credentials.email}, {"_id": 0})
    
    if not user:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid email or password"
        )
    
    if not verify_password(credentials.password, user["hashed_password"]):
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid email or password"
        )
    
    if not user.get("is_active", True):
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Account is deactivated"
        )
    
    # Update last login
    await db.users.update_one(
        {"email": credentials.email},
        {"$set": {"last_login": datetime.now(timezone.utc).isoformat()}}
    )
    
    # Fetch updated user
    user = await db.users.find_one({"email": credentials.email}, {"_id": 0})
    
    # Log session and activity
    await create_session(user["id"], user["email"], user["name"])
    await log_activity(user["id"], user["email"], user["name"], "login", "auth", "User logged in")
    
    # Create token
    access_token = create_access_token(
        data={"sub": user["id"], "email": user["email"], "role": user["role"]}
    )
    
    user_response = UserResponse(
        id=user["id"],
        email=user["email"],
        name=user["name"],
        role=user["role"],
        is_active=user.get("is_active", True),
        must_reset_password=user.get("must_reset_password", False),
        last_login=user.get("last_login"),
        created_at=user["created_at"]
    )
    
    return Token(access_token=access_token, user=user_response)


@router.post("/register", response_model=UserResponse)
async def register_user(user_data: UserCreate, current_user: dict = Depends(get_current_user)):
    """Create a new user (admin only) - sends temp password via email"""
    # Check if admin
    if current_user.get("role") != "admin":
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Only admins can create users"
        )
    
    # Check if user exists
    existing = await db.users.find_one({"email": user_data.email})
    if existing:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="User with this email already exists"
        )
    
    # Generate temp password
    temp_password = generate_temp_password()
    
    # Create user
    user = UserInDB(
        email=user_data.email,
        name=user_data.name,
        role=user_data.role,
        hashed_password=get_password_hash(temp_password),
        must_reset_password=True
    )
    
    await db.users.insert_one(user.model_dump())
    
    # Send welcome email with temp password
    smtp_settings = await db.smtp_settings.find_one({}, {"_id": 0})
    branding = await db.branding_settings.find_one({}, {"_id": 0})
    
    if smtp_settings:
        app_name = branding.get("app_name", "PromoGen") if branding else "PromoGen"
        await send_welcome_email(
            smtp_settings=smtp_settings,
            recipient_email=user_data.email,
            user_name=user_data.name,
            temp_password=temp_password,
            app_name=app_name
        )
    
    return UserResponse(
        id=user.id,
        email=user.email,
        name=user.name,
        role=user.role,
        is_active=user.is_active,
        must_reset_password=user.must_reset_password,
        last_login=user.last_login,
        created_at=user.created_at
    )


@router.post("/reset-password")
async def reset_password(passwords: PasswordReset, current_user: dict = Depends(get_current_user)):
    """Reset password for current user"""
    user = await db.users.find_one({"id": current_user["user_id"]}, {"_id": 0})
    
    if not user:
        raise HTTPException(status_code=404, detail="User not found")
    
    if not verify_password(passwords.current_password, user["hashed_password"]):
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Current password is incorrect"
        )
    
    # Update password and clear must_reset_password flag
    await db.users.update_one(
        {"id": current_user["user_id"]},
        {
            "$set": {
                "hashed_password": get_password_hash(passwords.new_password),
                "must_reset_password": False
            }
        }
    )
    
    return {"message": "Password updated successfully"}


@router.get("/me", response_model=UserResponse)
async def get_current_user_info(current_user: dict = Depends(get_current_user)):
    """Get current user information"""
    user = await db.users.find_one({"id": current_user["user_id"]}, {"_id": 0})
    
    if not user:
        raise HTTPException(status_code=404, detail="User not found")
    
    return UserResponse(
        id=user["id"],
        email=user["email"],
        name=user["name"],
        role=user["role"],
        is_active=user.get("is_active", True),
        must_reset_password=user.get("must_reset_password", False),
        last_login=user.get("last_login"),
        created_at=user["created_at"]
    )


@router.post("/setup-admin")
async def setup_admin():
    """Create initial admin user if no users exist"""
    count = await db.users.count_documents({})
    
    if count > 0:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Admin user already exists"
        )
    
    # Create default admin
    admin = UserInDB(
        email="admin@promogen.com",
        name="Admin",
        role="admin",
        hashed_password=get_password_hash("Admin@123"),
        must_reset_password=True
    )
    
    await db.users.insert_one(admin.model_dump())
    
    return {
        "message": "Admin user created",
        "email": "admin@promogen.com",
        "password": "Admin@123",
        "note": "Please change this password after first login"
    }
