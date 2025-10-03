# ADR 02: Push notification worker

- **Status**: Proposed
- **Date**: 2025-10-03
- **Decision drivers**:
  - We need a unified background task for transactional push notifications alongside the existing email worker.
  - Push delivery must work for Android, iOS, and web browsers while remaining tenant-aware and user-scoped.
  - Registrations have to survive across devices and allow invalid tokens to be retired automatically.

## Context

The platform currently supports transactional email via the `send_email` worker task, but there is no shared mechanism for delivering push notifications. Incoming product requirements call for mobile and browser notifications that respect per-tenant row level security and work for signed-in users rather than generic people records. Device tokens for Firebase Cloud Messaging (FCM) and Web Push subscriptions must be persisted so that Graphile Worker jobs can fan out delivery reliably.【F:worker/tasks/send_email.ts†L11-L43】

In addition to server-side delivery, productizing push notifications requires us to define the registration flow that runs on native apps and browsers. Mobile clients will continue to request permissions through the platform SDKs (Firebase for Android/iOS) and send refreshed FCM registration tokens to the backend whenever they change. For browsers we need a service worker that bootstraps the Web Push subscription (using `PushManager.subscribe`) once users accept the permission prompt and a thin piece of client-side code that POSTs the resulting endpoint + `p256dh`/`auth` keys to the API. The backend must expose tenant-aware endpoints/mutations so these registration payloads can be exchanged for `push_notification_channel` rows tied to the authenticated `user_id` instead of `person_id` and so that channels can be deactivated when users sign out or revoke permissions.

## Decision

We introduced a new `public.push_notification_channel` table that records each user’s delivery channels, the underlying provider (`fcm` or `web_push`), and metadata needed to address a device. The table carries tenant scoping, row-level security, and timestamps so that registrations can be managed through GraphQL while remaining idempotent in migrations.【F:migrations/current/1-current.sql†L1-L87】

A companion `send_push_notification` Graphile Worker task looks up active channels for the target user and:

- Sends Android and iOS notifications through Firebase Cloud Messaging using a service account loaded from `FIREBASE_SERVICE_ACCOUNT`.
- Dispatches browser notifications with the `web-push` library configured via `WEB_PUSH_VAPID_SUBJECT`, `WEB_PUSH_VAPID_PUBLIC_KEY`, and `WEB_PUSH_VAPID_PRIVATE_KEY`.
- Normalises payload data, updates `last_notified_at`, and deactivates channels that return permanent errors so stale tokens do not keep retrying.【F:worker/tasks/send_push_notification.ts†L1-L216】

We added `firebase-admin` and `web-push` as worker dependencies so the task can reach both delivery providers.【F:worker/package.json†L12-L36】

## Consequences

### Positive

- A single worker task now orchestrates mobile and browser push notifications for any registered user channel.
- Invalid device registrations are automatically marked inactive, reducing noise in future jobs.
- Row-level security keeps registrations tenant-scoped while still letting members manage their own channels through future APIs.

### Negative / Risks

- Operations must provision Firebase and VAPID credentials in production, and misconfiguration will cause jobs to fail fast.
- The worker now depends on sizeable FCM/Web Push SDKs, modestly increasing bundle size.
- Without a registration API yet, clients cannot create channels; a follow-up endpoint will be necessary before feature launch.
- Browser onboarding is brittle: registration requires HTTPS, a dedicated service worker, user permission prompts, and feature detection for legacy browsers that do not support Web Push. We will have to document fallbacks and guard client code accordingly.

## Follow-up actions

1. Expose GraphQL mutations or REST endpoints for clients to register and remove push channels.
2. Ship reference client flows:
   - Native: ensure Android/iOS builds call a shared helper that handles permission prompts, token refresh events, and background invalidation (e.g., delete the channel when FCM returns `messaging/registration-token-not-registered`).
   - Web: add a tenant-aware registration hook that boots the service worker, requests notification permission, creates a push subscription with the VAPID public key, and sends it to the backend.
3. Add observability around job success/failure counts to monitor deliverability.
4. Consider rate limiting or batching strategies if high-volume notifications become common.
