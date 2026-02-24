from pydantic import BaseModel, Field, ConfigDict
from typing import Optional
import uuid


class BrandingSettings(BaseModel):
    model_config = ConfigDict(extra="ignore")
    
    id: str = Field(default_factory=lambda: str(uuid.uuid4()))
    app_name: str = "PromoGen"
    logo_url: Optional[str] = None
    favicon_url: Optional[str] = None
    primary_color: str = "#E42E92"
    secondary_color: str = "#0099DC"
    success_color: str = "#84C343"
    warning_color: str = "#FFCC07"
    danger_color: str = "#FF4343"


class BrandingUpdate(BaseModel):
    app_name: Optional[str] = None
    logo_url: Optional[str] = None
    favicon_url: Optional[str] = None
    primary_color: Optional[str] = None
    secondary_color: Optional[str] = None
    success_color: Optional[str] = None
    warning_color: Optional[str] = None
    danger_color: Optional[str] = None


class SMTPSettings(BaseModel):
    model_config = ConfigDict(extra="ignore")
    
    id: str = Field(default_factory=lambda: str(uuid.uuid4()))
    provider: str = "resend"  # resend, smtp
    api_key: Optional[str] = None
    sender_email: str = "onboarding@resend.dev"
    sender_name: str = "PromoGen"
    # For traditional SMTP
    smtp_host: Optional[str] = None
    smtp_port: Optional[int] = None
    smtp_username: Optional[str] = None
    smtp_password: Optional[str] = None
    smtp_use_tls: bool = True


class SMTPUpdate(BaseModel):
    provider: Optional[str] = None
    api_key: Optional[str] = None
    sender_email: Optional[str] = None
    sender_name: Optional[str] = None
    smtp_host: Optional[str] = None
    smtp_port: Optional[int] = None
    smtp_username: Optional[str] = None
    smtp_password: Optional[str] = None
    smtp_use_tls: Optional[bool] = None


class SMTPTestRequest(BaseModel):
    recipient_email: str
