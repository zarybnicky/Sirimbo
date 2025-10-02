import { Stack, StackProps, CfnOutput } from 'aws-cdk-lib';
import { Construct } from 'constructs';
import * as ses from 'aws-cdk-lib/aws-ses';
import * as sns from 'aws-cdk-lib/aws-sns';
import * as subscriptions from 'aws-cdk-lib/aws-sns-subscriptions';

export interface SesNotificationsStackProps extends StackProps {
  /**
   * Fully-qualified domain that will send email via SES (for example mailing.rozpisovnik.cz).
   */
  readonly domainName: string;
  /**
   * Optional MAIL FROM subdomain. Must be a subdomain of domainName (for example bounce.mailing.rozpisovnik.cz).
   */
  readonly mailFromSubdomain?: string;
  /**
   * Optional email address to subscribe to the SNS feedback topic.
   */
  readonly notificationEmail?: string;
}

export class SesNotificationsStack extends Stack {
  constructor(scope: Construct, id: string, props: SesNotificationsStackProps) {
    super(scope, id, props);

    const configurationSetName = this.sanitisedConfigurationSetName(props.domainName);

    const feedbackTopic = new sns.Topic(this, 'SesFeedbackTopic', {
      displayName: 'Rozpisovnik SES feedback',
    });

    if (props.notificationEmail) {
      feedbackTopic.addSubscription(new subscriptions.EmailSubscription(props.notificationEmail));
    }

    const configurationSet = new ses.ConfigurationSet(this, 'MailingConfigurationSet', {
      configurationSetName,
      reputationMetrics: true,
      sendingEnabled: true,
      suppressionReasons: ses.SuppressionReasons.BOUNCES_AND_COMPLAINTS,
    });

    configurationSet.addEventDestination('FeedbackDestination', {
      destination: ses.EventDestination.snsTopic(feedbackTopic),
      events: [
        ses.EmailSendingEvent.SEND,
        ses.EmailSendingEvent.DELIVERY,
        ses.EmailSendingEvent.BOUNCE,
        ses.EmailSendingEvent.COMPLAINT,
        ses.EmailSendingEvent.REJECT,
      ],
    });

    const identity = new ses.EmailIdentity(this, 'MailingIdentity', {
      identity: ses.Identity.domain(props.domainName),
      configurationSet,
      dkimSigning: true,
      feedbackForwarding: false,
      mailFromDomain: props.mailFromSubdomain,
    });

    new CfnOutput(this, 'SesIdentityName', {
      value: identity.emailIdentityName,
      description: 'SES domain identity name (used for CLI commands).',
    });

    identity.dkimRecords.forEach((record, index) => {
      new CfnOutput(this, `DkimRecord${index + 1}`, {
        value: `${record.name} CNAME ${record.value}`,
        description: 'Create a CNAME record in Cloudflare for Easy DKIM.',
      });
    });

    new CfnOutput(this, 'ConfigurationSetName', {
      value: configurationSet.configurationSetName,
      description: 'Include this as X-SES-CONFIGURATION-SET when sending.',
    });

    new CfnOutput(this, 'FeedbackTopicArn', {
      value: feedbackTopic.topicArn,
      description: 'SNS topic that receives SES feedback events.',
    });

    if (props.mailFromSubdomain) {
      const region = Stack.of(this).region;
      new CfnOutput(this, 'MailFromMxRecord', {
        value: `${props.mailFromSubdomain} MX 10 feedback-smtp.${region}.amazonses.com`,
        description: 'Add an MX record for the MAIL FROM domain.',
      });
      new CfnOutput(this, 'MailFromTxtRecord', {
        value: `${props.mailFromSubdomain} TXT \"v=spf1 include:amazonses.com -all\"`,
        description: 'Add a TXT SPF record for the MAIL FROM domain.',
      });
    }
  }

  private sanitisedConfigurationSetName(domainName: string): string {
    const cleaned = domainName
      .replace(/[^a-zA-Z0-9]/g, '-')
      .replace(/-+/g, '-')
      .replace(/^-|-$/g, '');
    const suffix = '-config';
    const maxBaseLength = Math.max(1, 64 - suffix.length);
    const truncated = cleaned.slice(0, maxBaseLength);
    return `${truncated}${suffix}`;
  }
}
