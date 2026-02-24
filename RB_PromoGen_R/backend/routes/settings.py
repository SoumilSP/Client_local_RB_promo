from fastapi import APIRouter, HTTPException, Depends, UploadFile, File
from motor.motor_asyncio import AsyncIOMotorDatabase
import base64

from models.settings import (
    BrandingSettings, BrandingUpdate,
    SMTPSettings, SMTPUpdate, SMTPTestRequest
)
from utils.security import require_admin
from services.email_service import send_test_email

router = APIRouter(prefix="/settings", tags=["Settings"])

db: AsyncIOMotorDatabase = None

def set_db(database: AsyncIOMotorDatabase):
    global db
    db = database


# ==================== BRANDING ====================

@router.get("/branding", response_model=BrandingSettings)
async def get_branding():
    """Get branding settings (public)"""
    branding = await db.branding_settings.find_one({}, {"_id": 0})
    
    if not branding:
        # Return defaults
        return BrandingSettings()
    
    return BrandingSettings(**branding)


@router.put("/branding", response_model=BrandingSettings)
async def update_branding(updates: BrandingUpdate, current_user: dict = Depends(require_admin)):
    """Update branding settings (admin only)"""
    branding = await db.branding_settings.find_one({}, {"_id": 0})
    
    if not branding:
        # Create new with defaults
        branding = BrandingSettings().model_dump()
        await db.branding_settings.insert_one(branding)
    
    # Apply updates
    update_data = {k: v for k, v in updates.model_dump().items() if v is not None}
    
    if update_data:
        await db.branding_settings.update_one({}, {"$set": update_data})
    
    # Return updated
    updated = await db.branding_settings.find_one({}, {"_id": 0})
    return BrandingSettings(**updated)


@router.post("/branding/logo")
async def upload_logo(file: UploadFile = File(...), current_user: dict = Depends(require_admin)):
    """Upload logo image (admin only)"""
    if not file.content_type.startswith('image/'):
        raise HTTPException(status_code=400, detail="File must be an image")
    
    # Read and encode as base64 data URL
    content = await file.read()
    b64_data = base64.b64encode(content).decode('utf-8')
    data_url = f"data:{file.content_type};base64,{b64_data}"
    
    await db.branding_settings.update_one(
        {},
        {"$set": {"logo_url": data_url}},
        upsert=True
    )
    
    return {"message": "Logo uploaded successfully", "logo_url": data_url}


@router.post("/branding/favicon")
async def upload_favicon(file: UploadFile = File(...), current_user: dict = Depends(require_admin)):
    """Upload favicon image (admin only)"""
    if not file.content_type.startswith('image/'):
        raise HTTPException(status_code=400, detail="File must be an image")
    
    content = await file.read()
    b64_data = base64.b64encode(content).decode('utf-8')
    data_url = f"data:{file.content_type};base64,{b64_data}"
    
    await db.branding_settings.update_one(
        {},
        {"$set": {"favicon_url": data_url}},
        upsert=True
    )
    
    return {"message": "Favicon uploaded successfully", "favicon_url": data_url}


# ==================== SMTP ====================

@router.get("/smtp", response_model=SMTPSettings)
async def get_smtp(current_user: dict = Depends(require_admin)):
    """Get SMTP settings (admin only)"""
    smtp = await db.smtp_settings.find_one({}, {"_id": 0})
    
    if not smtp:
        return SMTPSettings()
    
    # Mask API key for security
    if smtp.get("api_key"):
        smtp["api_key"] = smtp["api_key"][:8] + "..." + smtp["api_key"][-4:]
    if smtp.get("smtp_password"):
        smtp["smtp_password"] = "********"
    
    return SMTPSettings(**smtp)


@router.put("/smtp", response_model=SMTPSettings)
async def update_smtp(updates: SMTPUpdate, current_user: dict = Depends(require_admin)):
    """Update SMTP settings (admin only)"""
    smtp = await db.smtp_settings.find_one({}, {"_id": 0})
    
    if not smtp:
        smtp = SMTPSettings().model_dump()
        await db.smtp_settings.insert_one(smtp)
    
    # Apply updates - skip masked password values
    update_data = {}
    for k, v in updates.model_dump().items():
        if v is not None:
            # Skip if password is masked placeholder
            if k == 'smtp_password' and v == '********':
                continue
            if k == 'api_key' and '...' in v:
                continue
            update_data[k] = v
    
    if update_data:
        await db.smtp_settings.update_one({}, {"$set": update_data})
    
    # Return updated (masked)
    updated = await db.smtp_settings.find_one({}, {"_id": 0})
    if updated.get("api_key"):
        updated["api_key"] = updated["api_key"][:8] + "..." + updated["api_key"][-4:]
    if updated.get("smtp_password"):
        updated["smtp_password"] = "********"
    
    return SMTPSettings(**updated)


@router.post("/smtp/test")
async def test_smtp(request: SMTPTestRequest, current_user: dict = Depends(require_admin)):
    """Test SMTP configuration by sending a test email"""
    smtp = await db.smtp_settings.find_one({}, {"_id": 0})
    
    if not smtp:
        raise HTTPException(
            status_code=400,
            detail="Email settings not configured. Please save your SMTP configuration first."
        )
    
    provider = smtp.get("provider", "smtp")
    
    # Validate configuration based on provider
    if provider == "resend":
        api_key = smtp.get("api_key", "")
        if not api_key or api_key.endswith("..."):
            raise HTTPException(
                status_code=400,
                detail="Resend API key not configured. Please enter a valid API key and save before testing."
            )
    else:  # SMTP provider
        if not smtp.get("smtp_host"):
            raise HTTPException(
                status_code=400,
                detail="SMTP Host is required. Please configure all SMTP settings and save before testing."
            )
        if not smtp.get("smtp_username"):
            raise HTTPException(
                status_code=400,
                detail="SMTP Username is required. Please configure all SMTP settings and save before testing."
            )
        if not smtp.get("smtp_password") or smtp.get("smtp_password") == "********":
            raise HTTPException(
                status_code=400,
                detail="SMTP Password is required. Please enter a valid password and save before testing."
            )
        if not smtp.get("sender_email"):
            raise HTTPException(
                status_code=400,
                detail="From Email is required. Please configure sender email and save before testing."
            )
    
    branding = await db.branding_settings.find_one({}, {"_id": 0})
    app_name = branding.get("app_name", "PromoGen") if branding else "PromoGen"
    
    result = await send_test_email(
        smtp_settings=smtp,
        recipient_email=request.recipient_email,
        app_name=app_name
    )
    
    if result.get("success"):
        return {"message": f"Test email sent successfully to {request.recipient_email}!", "email_id": result.get("email_id")}
    else:
        error_msg = result.get('error', 'Unknown error')
        # Make error messages more user-friendly
        if "authentication" in error_msg.lower() or "invalid" in error_msg.lower():
            raise HTTPException(status_code=400, detail="Authentication failed. Please verify your SMTP credentials are correct.")
        elif "connection" in error_msg.lower() or "connect" in error_msg.lower():
            raise HTTPException(status_code=400, detail="Could not connect to SMTP server. Please verify the host and port are correct.")
        elif "api key" in error_msg.lower():
            raise HTTPException(status_code=400, detail="Invalid API key. Please verify your Resend API key is correct.")
        else:
            raise HTTPException(status_code=500, detail=f"Failed to send test email: {error_msg}")
