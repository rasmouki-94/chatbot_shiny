import smtplib
from email.message import EmailMessage


def send_email_smtp(
    smtp_host,
    smtp_port,
    smtp_user,
    smtp_pass,
    mail_from,
    mail_to,
    subject,
    body_text,
):
    msg = EmailMessage()
    msg["Subject"] = subject
    msg["From"] = mail_from
    msg["To"] = mail_to
    msg.set_content(body_text)

    with smtplib.SMTP_SSL(smtp_host, int(smtp_port)) as server:
        server.login(smtp_user, smtp_pass)
        server.send_message(msg)

    return True
