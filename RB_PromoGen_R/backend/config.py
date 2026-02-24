import os
from dotenv import load_dotenv
from pathlib import Path

ROOT_DIR = Path(__file__).parent
load_dotenv(ROOT_DIR / '.env')

class Settings:
    MONGO_URL: str = os.environ.get('MONGO_URL', 'mongodb://localhost:27017')
    DB_NAME: str = os.environ.get('DB_NAME', 'promogen_db')
    
    # JWT Settings
    JWT_SECRET_KEY: str = os.environ.get('JWT_SECRET_KEY', 'super-secret-key-change-in-production')
    JWT_ALGORITHM: str = 'HS256'
    ACCESS_TOKEN_EXPIRE_MINUTES: int = 60 * 24  # 24 hours
    
    # SMTP Settings (loaded from DB, fallback to env)
    RESEND_API_KEY: str = os.environ.get('RESEND_API_KEY', '')
    SENDER_EMAIL: str = os.environ.get('SENDER_EMAIL', 'onboarding@resend.dev')
    
    # CORS
    CORS_ORIGINS: str = os.environ.get('CORS_ORIGINS', '*')

settings = Settings()
