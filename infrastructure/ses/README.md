# Amazon SES configuration set for Rozpisovnik

This package contains an AWS CDK stack that provisions the minimum Amazon SES resources
needed to send bulk transactional emails for `mailing.rozpisovnik.cz` while keeping costs
down (shared IP pools only) and capturing delivery/bounce/complaint notifications in a
single SNS topic.

The stack intentionally does **not** create DNS records because the domain is hosted in
Cloudflare. After the stack is deployed you must copy the CloudFormation outputs and add
the required DNS records manually.

## What the stack creates

* An SES domain identity for `mailing.rozpisovnik.cz` with Easy DKIM enabled and feedback
  forwarding disabled (bounce/complaint events are routed through SNS instead).
* A configuration set with SNS event destination for delivery, bounce, complaint, reject,
  and send events.
* A shared SNS topic that can fan out notifications (optionally with an email subscription).
* Optional MAIL FROM records when `mailFromSubdomain` is set (defaults to
  `bounce.mailing.rozpisovnik.cz`).

The resources are parameterised through CDK context:

| Context key           | Default value                        | Purpose |
| --------------------- | ------------------------------------ | ------- |
| `domainName`          | `mailing.rozpisovnik.cz`             | SES sending identity |
| `mailFromSubdomain`   | `bounce.mailing.rozpisovnik.cz`      | Custom MAIL FROM domain |
| `notificationEmail`   | (empty)                              | Optional email subscription to the SNS topic |

Override the values with `--context` flags or by editing `cdk.json` before synthesising.

## Prerequisites

1. **AWS account with CDK bootstrapped** in the region you want to send mail from (for the
   Czech audience `eu-central-1` is usually the closest). Bootstrap once with:
   ```bash
   yarn workspace @rozpisovnik/ses-infra cdk bootstrap
   ```
2. **SES production access.** New accounts are limited to the sandbox (only verified
   recipients). Request production access in the AWS console under SES → Account dashboard
   → "Move out of the sandbox" and provide expected sending volumes and use case details.
3. **IAM permissions.** The deployer needs rights to manage SES, SNS, and CloudFormation.
4. **AWS credentials available in your shell** (environment variables, `~/.aws/credentials`,
   or SSO).

## Deploying

```bash
yarn workspace @rozpisovnik/ses-infra synth   # optional: review CloudFormation
yarn workspace @rozpisovnik/ses-infra deploy  # deploys the stack
```

The deployment prints CloudFormation outputs containing the exact DNS records to add in
Cloudflare. Create the following records as "DNS only" (grey cloud) so Cloudflare does not
proxy them:

1. **Easy DKIM CNAMEs** – three CNAME records, one for each output named `DkimRecordX`.
2. **Domain verification TXT** – SES still requires the `_amazonses.mailing.rozpisovnik.cz`
   TXT record even though the CDK API does not expose it. Retrieve the value with:
   ```bash
   aws sesv2 get-email-identity --email-identity mailing.rozpisovnik.cz \
     --query 'VerificationAttributes.CurrentVerificationToken' --output text
   ```
   Then create `_amazonses.mailing.rozpisovnik.cz` → `TXT` with the returned token.
3. **MAIL FROM MX/TXT** – if `mailFromSubdomain` is populated, add the MX and TXT records
   printed as `MailFromMxRecord` and `MailFromTxtRecord`.
4. **SPF/DMARC alignment** – create/adjust TXT records so that:
   * `mailing.rozpisovnik.cz` has `v=spf1 include:amazonses.com -all`
   * `_dmarc.mailing.rozpisovnik.cz` (new) has something like
     `v=DMARC1; p=quarantine; rua=mailto:postmaster@rozpisovnik.cz; ruf=mailto:postmaster@rozpisovnik.cz`
     (tune the policy to match your appetite for risk).

Cloudflare must leave these records unproxied; otherwise Amazon cannot resolve them.
Propagation can take a few minutes – verify status in the SES console under
Verified identities.

## Wiring notifications

The SNS topic ARN is exposed as the `FeedbackTopicArn` output. Subscribe whichever
consumers you need:

* add `notificationEmail` in `cdk.json` to receive raw notifications by email,
* subscribe a webhook (via HTTPS) or an SQS queue for programmatic handling,
* or fan into an EventBridge rule for alerting.

To have SES publish events to the destination you **must** send messages with the created
configuration set. With SMTP that means adding the header
`X-SES-CONFIGURATION-SET: <ConfigurationSetName>`; with the API use the
`ConfigurationSetName` parameter. Set the worker/process environment variable
`SES_CONFIGURATION_SET` to the value emitted as the `ConfigurationSetName`
output so application mailers can attach the header automatically.

## Using the SMTP endpoint

Create or reuse IAM SMTP credentials from the SES console (under Email sending → SMTP
settings). The stack does not generate credentials automatically. Configure your mailer to
use the region-specific SMTP endpoint (for example `email-smtp.eu-central-1.amazonaws.com`),
Port 587 with STARTTLS, and the `X-SES-CONFIGURATION-SET` header. Keep the credentials in
Secrets Manager or your existing secret store – they are long-lived IAM user passwords. If
you set `SES_CONFIGURATION_SET` alongside your SMTP credentials, the background worker
will inject the header automatically.

## Common pitfalls and gotchas

* **Cloudflare proxied records break SES.** Ensure every SES-related DNS entry is set to
  "DNS only". Orange-clouded CNAMEs will never validate.
* **MAIL FROM domain cannot receive email.** It needs a dedicated subdomain (e.g.
  `bounce.mailing.rozpisovnik.cz`) and should not be re-used elsewhere.
* **Deliverability depends on domain reputation.** Even with DKIM/SPF/DMARC, expect a warm-up
  period. Keep complaint rates below 0.1% to avoid throttling.
* **SES sandbox limits** apply until AWS approves your production access request.
* **Shared IP pools** are cost-effective but sensitive to other tenants. Monitor bounces via
  the SNS topic and consider pausing campaigns if metrics degrade.
* **Regional choice matters.** Stick to one region for sending; SMTP credentials and the
  configuration set are region-specific.

## Cleaning up

Remove the stack when you no longer need SES in this account:

```bash
yarn workspace @rozpisovnik/ses-infra destroy
```

Delete the DNS records afterwards to avoid leaving stale verification tokens around.
