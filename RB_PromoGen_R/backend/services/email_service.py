import asyncio
import logging
import smtplib
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from typing import Optional

logger = logging.getLogger(__name__)


async def send_email_resend(
    api_key: str,
    sender_email: str,
    recipient_email: str,
    subject: str,
    html_content: str
) -> dict:
    """Send email using Resend API"""
    try:
        import resend
        resend.api_key = api_key
        
        params = {
            "from": sender_email,
            "to": [recipient_email],
            "subject": subject,
            "html": html_content
        }
        
        email = await asyncio.to_thread(resend.Emails.send, params)
        return {"success": True, "email_id": email.get("id")}
    except Exception as e:
        logger.error(f"Failed to send email via Resend: {str(e)}")
        return {"success": False, "error": str(e)}


async def send_email_smtp(
    smtp_host: str,
    smtp_port: int,
    smtp_username: str,
    smtp_password: str,
    sender_email: str,
    sender_name: str,
    recipient_email: str,
    subject: str,
    html_content: str,
    use_tls: bool = True
) -> dict:
    """Send email using traditional SMTP"""
    try:
        msg = MIMEMultipart('alternative')
        msg['Subject'] = subject
        msg['From'] = f"{sender_name} <{sender_email}>"
        msg['To'] = recipient_email
        
        html_part = MIMEText(html_content, 'html')
        msg.attach(html_part)
        
        def send_sync():
            server = None
            try:
                # Port 465 uses SSL directly, other ports typically use STARTTLS
                if smtp_port == 465:
                    server = smtplib.SMTP_SSL(smtp_host, smtp_port, timeout=30)
                else:
                    server = smtplib.SMTP(smtp_host, smtp_port, timeout=30)
                    server.ehlo()
                    if use_tls:
                        server.starttls()
                        server.ehlo()
                
                server.login(smtp_username, smtp_password)
                server.sendmail(sender_email, recipient_email, msg.as_string())
            finally:
                if server:
                    try:
                        server.quit()
                    except:
                        pass
        
        await asyncio.to_thread(send_sync)
        return {"success": True}
    except smtplib.SMTPAuthenticationError as e:
        logger.error(f"SMTP Authentication failed: {str(e)}")
        return {"success": False, "error": f"Authentication failed: {str(e)}. For Gmail, use an App Password instead of your regular password."}
    except smtplib.SMTPConnectError as e:
        logger.error(f"SMTP Connection failed: {str(e)}")
        return {"success": False, "error": f"Connection failed: {str(e)}"}
    except smtplib.SMTPException as e:
        logger.error(f"SMTP Error: {str(e)}")
        return {"success": False, "error": str(e)}
    except Exception as e:
        logger.error(f"Failed to send email via SMTP: {str(e)}")
        return {"success": False, "error": str(e)}


async def send_email(
    smtp_settings: dict,
    recipient_email: str,
    subject: str,
    html_content: str
) -> dict:
    """Send email using configured provider (Resend or SMTP)"""
    provider = smtp_settings.get("provider", "resend")
    
    if provider == "resend" and smtp_settings.get("api_key"):
        return await send_email_resend(
            api_key=smtp_settings["api_key"],
            sender_email=smtp_settings.get("sender_email", "onboarding@resend.dev"),
            recipient_email=recipient_email,
            subject=subject,
            html_content=html_content
        )
    elif provider == "smtp" and smtp_settings.get("smtp_host"):
        return await send_email_smtp(
            smtp_host=smtp_settings["smtp_host"],
            smtp_port=smtp_settings.get("smtp_port", 587),
            smtp_username=smtp_settings.get("smtp_username", ""),
            smtp_password=smtp_settings.get("smtp_password", ""),
            sender_email=smtp_settings.get("sender_email", ""),
            sender_name=smtp_settings.get("sender_name", "PromoGen"),
            recipient_email=recipient_email,
            subject=subject,
            html_content=html_content,
            use_tls=smtp_settings.get("smtp_use_tls", True)
        )
    else:
        return {"success": False, "error": "Email not configured. Please configure SMTP or Resend API in Admin Settings."}


async def send_welcome_email(
    smtp_settings: dict,
    recipient_email: str,
    user_name: str,
    temp_password: str,
    app_name: str = "PromoGen"
) -> dict:
    """Send welcome email with temporary password"""
    subject = f"Welcome to {app_name} - Your Account"
    html_content = f"""
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body style="font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; line-height: 1.6; color: #1a1a1a; max-width: 600px; margin: 0 auto; padding: 20px;">
        <div style="background: linear-gradient(135deg, #E42E92, #0099DC); padding: 32px; text-align: center; border-radius: 12px 12px 0 0;">
            <h1 style="color: white; margin: 0; font-size: 24px; font-weight: 700;">{app_name}</h1>
        </div>
        <div style="background: #ffffff; padding: 32px; border: 1px solid #e5e5e5; border-top: 0; border-radius: 0 0 12px 12px;">
            <h2 style="color: #E42E92; margin-top: 0; font-size: 20px;">Welcome, {user_name}!</h2>
            <p style="color: #4a4a4a;">Your account has been created. Use these credentials to sign in:</p>
            <div style="background: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 4px solid #E42E92; margin: 24px 0;">
                <p style="margin: 4px 0; color: #1a1a1a;"><strong>Email:</strong> {recipient_email}</p>
                <p style="margin: 4px 0; color: #1a1a1a;"><strong>Temporary Password:</strong> <code style="background: #e9ecef; padding: 4px 8px; border-radius: 4px; font-size: 14px;">{temp_password}</code></p>
            </div>
            <p style="color: #dc3545; font-size: 14px;"><strong>Important:</strong> You will need to change your password on first login.</p>
            <hr style="border: none; border-top: 1px solid #e5e5e5; margin: 24px 0;">
            <p style="color: #6c757d; font-size: 12px; margin-bottom: 0;">This is an automated message from {app_name}.</p>
        </div>
    </body>
    </html>
    """
    return await send_email(smtp_settings, recipient_email, subject, html_content)


async def send_test_email(
    smtp_settings: dict,
    recipient_email: str,
    app_name: str = "PromoGen"
) -> dict:
    """Send test email to verify configuration"""
    subject = f"{app_name} - Test Email"
    html_content = f"""
    <!DOCTYPE html>
    <html>
    <head><meta charset="UTF-8"></head>
    <body style="font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; padding: 24px;">
        <div style="background: #22c55e; color: white; padding: 20px; text-align: center; border-radius: 8px; margin-bottom: 20px;">
            <h2 style="margin: 0;">✓ Email Configuration Successful</h2>
        </div>
        <p>This is a test email from <strong>{app_name}</strong>.</p>
        <p>If you received this email, your email settings are configured correctly.</p>
        <hr style="border: none; border-top: 1px solid #e5e5e5; margin: 20px 0;">
        <p style="color: #6c757d; font-size: 12px;">Sent from {app_name} Admin Panel</p>
    </body>
    </html>
    """
    return await send_email(smtp_settings, recipient_email, subject, html_content)
