#!/usr/bin/env node
import { App } from 'aws-cdk-lib';
import { SesNotificationsStack, SesNotificationsStackProps } from '../lib/ses-notifications-stack';

const app = new App();

const domainName = app.node.tryGetContext('domainName');
if (!domainName || typeof domainName !== 'string') {
  throw new Error('Set "domainName" in cdk.json or via --context to the SES sending domain (for example mailing.rozpisovnik.cz).');
}

const mailFromSubdomain = app.node.tryGetContext('mailFromSubdomain');
const notificationEmail = app.node.tryGetContext('notificationEmail');

const stackProps: SesNotificationsStackProps = {
  env: {
    account: process.env.CDK_DEFAULT_ACCOUNT,
    region: process.env.CDK_DEFAULT_REGION ?? 'eu-central-1',
  },
  domainName,
  mailFromSubdomain:
    typeof mailFromSubdomain === 'string' && mailFromSubdomain.length > 0 ? mailFromSubdomain : undefined,
  notificationEmail: typeof notificationEmail === 'string' && notificationEmail.length > 0 ? notificationEmail : undefined,
};

new SesNotificationsStack(app, 'SesNotificationsStack', stackProps);
