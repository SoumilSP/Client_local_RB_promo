from pydantic import BaseModel, EmailStr, Field, ConfigDict
from typing import Optional
from datetime import datetime
import uuid


class UserBase(BaseModel):
    email: EmailStr
    name: str
    role: str = "user"  # "admin" or "user"


class UserCreate(BaseModel):
    email: EmailStr
    name: str
    role: str = "user"


class UserLogin(BaseModel):
    email: EmailStr
    password: str


class UserResponse(BaseModel):
    model_config = ConfigDict(extra="ignore")
    
    id: str
    email: EmailStr
    name: str
    role: str
    is_active: bool = True
    must_reset_password: bool = True
    last_login: Optional[str] = None
    created_at: str


class UserInDB(BaseModel):
    id: str = Field(default_factory=lambda: str(uuid.uuid4()))
    email: EmailStr
    name: str
    role: str = "user"
    hashed_password: str
    is_active: bool = True
    must_reset_password: bool = True
    last_login: Optional[str] = None
    created_at: str = Field(default_factory=lambda: datetime.utcnow().isoformat())


class PasswordReset(BaseModel):
    current_password: str
    new_password: str


class Token(BaseModel):
    access_token: str
    token_type: str = "bearer"
    user: UserResponse


class TokenData(BaseModel):
    user_id: Optional[str] = None
    email: Optional[str] = None
