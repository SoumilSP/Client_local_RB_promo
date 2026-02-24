from fastapi import APIRouter, HTTPException, Depends, Request
from typing import List, Optional
from pydantic import BaseModel, EmailStr
from motor.motor_asyncio import AsyncIOMotorDatabase

from models.user import UserResponse
from utils.security import get_current_user, require_admin

router = APIRouter(prefix="/users", tags=["Users"])

db: AsyncIOMotorDatabase = None
audit_log_activity = None  # Will be set from server.py

def set_db(database: AsyncIOMotorDatabase):
    global db
    db = database

def set_audit_logger(log_fn):
    global audit_log_activity
    audit_log_activity = log_fn


class UserUpdate(BaseModel):
    name: Optional[str] = None
    email: Optional[EmailStr] = None
    role: Optional[str] = None
    is_active: Optional[bool] = None


@router.get("/", response_model=List[UserResponse])
async def list_users(current_user: dict = Depends(require_admin)):
    """List all users (admin only)"""
    users = await db.users.find({}, {"_id": 0, "hashed_password": 0}).to_list(1000)
    return [UserResponse(**u) for u in users]


@router.get("/{user_id}", response_model=UserResponse)
async def get_user(user_id: str, current_user: dict = Depends(require_admin)):
    """Get user by ID (admin only)"""
    user = await db.users.find_one({"id": user_id}, {"_id": 0, "hashed_password": 0})
    
    if not user:
        raise HTTPException(status_code=404, detail="User not found")
    
    return UserResponse(**user)


@router.put("/{user_id}", response_model=UserResponse)
async def update_user(user_id: str, user_update: UserUpdate, current_user: dict = Depends(require_admin)):
    """Update user details (admin only)"""
    user = await db.users.find_one({"id": user_id}, {"_id": 0})
    
    if not user:
        raise HTTPException(status_code=404, detail="User not found")
    
    # Store old values for audit
    old_values = {}
    
    # Build update dict with only provided fields
    update_data = {}
    if user_update.name is not None and user_update.name != user.get("name"):
        old_values["name"] = user.get("name")
        update_data["name"] = user_update.name
    if user_update.email is not None and user_update.email != user.get("email"):
        # Check if email is already taken by another user
        existing = await db.users.find_one({"email": user_update.email, "id": {"$ne": user_id}})
        if existing:
            raise HTTPException(status_code=400, detail="Email already in use")
        old_values["email"] = user.get("email")
        update_data["email"] = user_update.email
    if user_update.role is not None and user_update.role != user.get("role"):
        if user_update.role not in ["admin", "user", "viewer"]:
            raise HTTPException(status_code=400, detail="Invalid role")
        old_values["role"] = user.get("role")
        update_data["role"] = user_update.role
    if user_update.is_active is not None and user_update.is_active != user.get("is_active"):
        # Prevent admin from deactivating themselves
        if user_id == current_user["user_id"] and not user_update.is_active:
            raise HTTPException(status_code=400, detail="Cannot deactivate your own account")
        old_values["is_active"] = user.get("is_active")
        update_data["is_active"] = user_update.is_active
    
    if not update_data:
        raise HTTPException(status_code=400, detail="No fields to update")
    
    await db.users.update_one({"id": user_id}, {"$set": update_data})
    
    # Log activity
    if audit_log_activity:
        await audit_log_activity(
            user_id=current_user["user_id"],
            user_email=current_user.get("email", ""),
            user_name=current_user.get("name", "Admin"),
            module="Users",
            action="update",
            object_type="User",
            object_id=user_id,
            object_name=user.get("name", user.get("email")),
            details=f"Updated user: {user.get('email')}",
            old_values=old_values,
            new_values=update_data
        )
    
    updated_user = await db.users.find_one({"id": user_id}, {"_id": 0, "hashed_password": 0})
    return UserResponse(**updated_user)


@router.patch("/{user_id}/toggle-status")
async def toggle_user_status(user_id: str, current_user: dict = Depends(require_admin)):
    """Toggle user active status (admin only)"""
    user = await db.users.find_one({"id": user_id}, {"_id": 0})
    
    if not user:
        raise HTTPException(status_code=404, detail="User not found")
    
    # Prevent admin from deactivating themselves
    if user_id == current_user["user_id"]:
        raise HTTPException(status_code=400, detail="Cannot deactivate your own account")
    
    old_status = user.get("is_active", True)
    new_status = not old_status
    await db.users.update_one({"id": user_id}, {"$set": {"is_active": new_status}})
    
    # Log activity
    if audit_log_activity:
        await audit_log_activity(
            user_id=current_user["user_id"],
            user_email=current_user.get("email", ""),
            user_name=current_user.get("name", "Admin"),
            module="Users",
            action="update",
            object_type="User",
            object_id=user_id,
            object_name=user.get("name", user.get("email")),
            details=f"{'Activated' if new_status else 'Deactivated'} user: {user.get('email')}",
            old_values={"is_active": old_status},
            new_values={"is_active": new_status}
        )
    
    return {"message": f"User {'activated' if new_status else 'deactivated'}", "is_active": new_status}


@router.delete("/{user_id}")
async def delete_user(user_id: str, current_user: dict = Depends(require_admin)):
    """Delete user (admin only)"""
    # Prevent admin from deleting themselves
    if user_id == current_user["user_id"]:
        raise HTTPException(status_code=400, detail="Cannot delete your own account")
    
    # Get user info for audit log before deletion
    user = await db.users.find_one({"id": user_id}, {"_id": 0, "hashed_password": 0})
    
    result = await db.users.delete_one({"id": user_id})
    
    if result.deleted_count == 0:
        raise HTTPException(status_code=404, detail="User not found")
    
    # Log activity
    if audit_log_activity and user:
        await audit_log_activity(
            user_id=current_user["user_id"],
            user_email=current_user.get("email", ""),
            user_name=current_user.get("name", "Admin"),
            module="Users",
            action="delete",
            object_type="User",
            object_id=user_id,
            object_name=user.get("name", user.get("email")),
            details=f"Deleted user: {user.get('email')}",
            old_values={"name": user.get("name"), "email": user.get("email"), "role": user.get("role")}
        )
    
    return {"message": "User deleted successfully"}
