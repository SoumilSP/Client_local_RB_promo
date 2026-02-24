from fastapi import APIRouter, Depends, Request
from typing import List, Optional, Dict, Any
from motor.motor_asyncio import AsyncIOMotorDatabase
from datetime import datetime
import json

from models.audit import UserActivity, UserSession, ActivityLog, SessionLog
from utils.security import require_admin

router = APIRouter(prefix="/audit", tags=["Audit"])

db: AsyncIOMotorDatabase = None

def set_db(database: AsyncIOMotorDatabase):
    global db
    db = database


async def log_activity(
    user_id: str,
    user_email: str,
    user_name: str,
    module: str,
    action: str,
    object_type: str = None,
    object_id: str = None,
    object_name: str = None,
    details: str = None,
    old_values: Dict[str, Any] = None,
    new_values: Dict[str, Any] = None,
    ip_address: str = None,
    user_agent: str = None
):
    """Log user activity (excluding login/logout which go to sessions)"""
    activity = UserActivity(
        user_id=user_id,
        user_email=user_email,
        user_name=user_name,
        module=module,
        action=action,
        object_type=object_type,
        object_id=object_id,
        object_name=object_name,
        details=details,
        old_values=old_values,
        new_values=new_values,
        ip_address=ip_address,
        user_agent=user_agent
    )
    await db.user_activities.insert_one(activity.model_dump())
    return activity


async def create_session(
    user_id: str,
    user_email: str,
    user_name: str,
    ip_address: str = None,
    user_agent: str = None
):
    """Create a new user session"""
    # Parse device from user agent
    device = "Unknown"
    if user_agent:
        ua_lower = user_agent.lower()
        if "mobile" in ua_lower or "android" in ua_lower or "iphone" in ua_lower:
            device = "Mobile"
        elif "tablet" in ua_lower or "ipad" in ua_lower:
            device = "Tablet"
        else:
            device = "Desktop"
    
    session = UserSession(
        user_id=user_id,
        user_email=user_email,
        user_name=user_name,
        login_time=datetime.utcnow().isoformat(),
        ip_address=ip_address,
        user_agent=user_agent,
        device=device,
        is_active=True
    )
    await db.user_sessions.insert_one(session.model_dump())
    return session


async def end_session(user_id: str):
    """End active session for user"""
    await db.user_sessions.update_many(
        {"user_id": user_id, "is_active": True},
        {"$set": {"is_active": False, "logout_time": datetime.utcnow().isoformat()}}
    )


@router.get("/activities", response_model=List[ActivityLog])
async def get_activities(
    limit: int = 500,
    user_email: Optional[str] = None,
    module: Optional[str] = None,
    action: Optional[str] = None,
    current_user: dict = Depends(require_admin)
):
    """Get user activity logs (admin only) - excludes login/logout"""
    query = {}
    if user_email:
        query["user_email"] = user_email
    if module:
        query["module"] = module
    if action:
        query["action"] = action
    
    # Exclude login/logout from activity report (they're in sessions)
    query["action"] = {"$nin": ["login", "logout"]}
    
    activities = await db.user_activities.find(
        query, 
        {"_id": 0}
    ).sort("timestamp", -1).limit(limit).to_list(limit)
    
    result = []
    for a in activities:
        result.append(ActivityLog(
            id=a.get("id", ""),
            user_email=a.get("user_email", ""),
            user_name=a.get("user_name", ""),
            module=a.get("module", a.get("resource", "Unknown")),
            action=a.get("action", ""),
            object_type=a.get("object_type"),
            object_id=a.get("object_id"),
            object_name=a.get("object_name"),
            details=a.get("details"),
            old_values=a.get("old_values"),
            new_values=a.get("new_values"),
            timestamp=a.get("timestamp", "")
        ))
    
    return result


@router.get("/modules")
async def get_modules(current_user: dict = Depends(require_admin)):
    """Get list of modules for filtering"""
    modules = await db.user_activities.distinct("module")
    return [m for m in modules if m]


@router.get("/sessions", response_model=List[SessionLog])
async def get_sessions(
    limit: int = 500,
    user_id: Optional[str] = None,
    user_email: Optional[str] = None,
    active_only: bool = False,
    current_user: dict = Depends(require_admin)
):
    """Get user session logs (admin only)"""
    query = {}
    if user_id:
        query["user_id"] = user_id
    if user_email:
        query["user_email"] = user_email
    if active_only:
        query["is_active"] = True
    
    sessions = await db.user_sessions.find(
        query, 
        {"_id": 0}
    ).sort("login_time", -1).limit(limit).to_list(limit)
    
    result = []
    for s in sessions:
        duration = None
        if s.get("login_time"):
            try:
                login_str = s["login_time"]
                if login_str.endswith('Z'):
                    login_str = login_str[:-1] + '+00:00'
                elif '+' not in login_str and '-' not in login_str[-6:]:
                    login_str = login_str + '+00:00'
                login = datetime.fromisoformat(login_str)
                
                if s.get("logout_time"):
                    logout_str = s["logout_time"]
                    if logout_str.endswith('Z'):
                        logout_str = logout_str[:-1] + '+00:00'
                    elif '+' not in logout_str and '-' not in logout_str[-6:]:
                        logout_str = logout_str + '+00:00'
                    logout = datetime.fromisoformat(logout_str)
                    duration = int((logout - login).total_seconds() / 60)
                elif s.get("is_active"):
                    duration = int((datetime.utcnow() - login.replace(tzinfo=None)).total_seconds() / 60)
            except Exception:
                duration = None
        
        result.append(SessionLog(
            id=s["id"],
            user_email=s["user_email"],
            user_name=s["user_name"],
            login_time=s["login_time"],
            logout_time=s.get("logout_time"),
            duration_minutes=duration,
            ip_address=s.get("ip_address"),
            device=s.get("device", "Unknown"),
            is_active=s.get("is_active", False)
        ))
    
    return result


@router.get("/stats")
async def get_audit_stats(current_user: dict = Depends(require_admin)):
    """Get audit statistics"""
    total_activities = await db.user_activities.count_documents({"action": {"$nin": ["login", "logout"]}})
    total_sessions = await db.user_sessions.count_documents({})
    active_sessions = await db.user_sessions.count_documents({"is_active": True})
    
    # Get activity counts by module
    pipeline = [
        {"$match": {"action": {"$nin": ["login", "logout"]}}},
        {"$group": {"_id": "$module", "count": {"$sum": 1}}},
        {"$sort": {"count": -1}},
        {"$limit": 10}
    ]
    module_stats = await db.user_activities.aggregate(pipeline).to_list(10)
    
    return {
        "total_activities": total_activities,
        "total_sessions": total_sessions,
        "active_sessions": active_sessions,
        "top_modules": [{"module": a["_id"], "count": a["count"]} for a in module_stats]
    }
