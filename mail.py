#!/usr/bin/env python

import os
import smtplib

from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText

user = os.environ['MAIL_USER']
pwd = os.environ['MAIL_PASS']
text = """\
Vážení členové,

dovolujeme si Vás upozornit, že na webu tkolymp.cz v sekci Nástěnka byly zveřejněny platební informace k platbě členských příspěvků s termínem 21. 2. 2020. Prosíme Vás o dodržení termínu a upozorňujeme na navýšení členského příspěvku při platbě po termínu o 500 Kč.

Jakékoliv další informace k platbám členských příspěvků Vám poskytne Mgr. Lucie Benýšková, lucie.benyskova@tkolymp.cz, 777 154 730.

S pozdravem
Miroslav Hýža, předseda TK
"""
with open('hakka-email.html', 'r') as f:
    html = f.read()

with open('emails.txt', 'r') as f:
    emails = f.read().split('\n')
    print("Emails %d" % len(emails))

def make_message(fro, to):
    msg = MIMEMultipart('alternative')
    msg['Subject'] = "Členské příspěvky TK Olymp"
    msg['From'] = fro
    msg['To'] = to
    msg.attach(MIMEText(text, 'plain'))
    msg.attach(MIMEText(html, 'html'))
    return msg.as_string();

with smtplib.SMTP('mail.zarybnicky.com', 587) as s:
    s.starttls()
    s.login(user, pwd)
    print("Logged in");
    for to in emails:
        s.sendmail(user, to, make_message(user, to))
        print("Sent to %s" % to)
