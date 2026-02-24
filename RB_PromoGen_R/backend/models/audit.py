from pydantic import BaseModel, Field, ConfigDict
from typing import Optional, List, Dict, Any
from datetime import datetime
import uuid


class UserActivity(BaseModel):
    model_config = ConfigDict(extra="ignore")
    
    id: str = Field(default_factory=lambda: str(uuid.uuid4()))
    user_id: str
    user_email: str
    user_name: str
    module: str  # Auth, Users, Settings, Promo, Reports, etc.
    action: str  # create, update, delete, export, etc.
    object_type: Optional[str] = None  # User, Settings, Report, etc.
    object_id: Optional[str] = None  # ID of the affected object
    object_name: Optional[str] = None  # Display name of the object
    details: Optional[str] = None  # Human readable summary
    old_values: Optional[Dict[str, Any]] = None  # Previous values for updates
    new_values: Optional[Dict[str, Any]] = None  # New values for updates
    ip_address: Optional[str] = None
    user_agent: Optional[str] = None
    timestamp: str = Field(default_factory=lambda: datetime.utcnow().isoformat())


class UserSession(BaseModel):
    model_config = ConfigDict(extra="ignore")
    
    id: str = Field(default_factory=lambda: str(uuid.uuid4()))
    user_id: str
    user_email: str
    user_name: str
    login_time: str
    logout_time: Optional[str] = None
    ip_address: Optional[str] = None
    user_agent: Optional[str] = None
    device: Optional[str] = None
    is_active: bool = True


class ActivityLog(BaseModel):
    id: str
    user_email: str
    user_name: str
    module: str
    action: str
    object_type: Optional[str] = None
    object_id: Optional[str] = None
    object_name: Optional[str] = None
    details: Optional[str] = None
    old_values: Optional[Dict[str, Any]] = None
    new_values: Optional[Dict[str, Any]] = None
    timestamp: str


class SessionLog(BaseModel):
    id: str
    user_email: str
    user_name: str
    login_time: str
    logout_time: Optional[str]
    duration_minutes: Optional[int]
    ip_address: Optional[str]
    device: Optional[str] = None
    is_active: bool
