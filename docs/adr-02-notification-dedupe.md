# ADR 02: Notification Deduplication Infrastructure

## Status
Accepted

## Context
- Announcements can be republished multiple times while only their audience or delivery schedule changes. Recipients should not be spammed with duplicate notifications when the announcement body stays the same.
- We anticipate future notification use cases (e.g., pending membership approvals, lesson registrations) that should reuse a shared infrastructure.
- The codebase previously lacked a tenant-scoped notification log to drive in-app or push notifications and no mechanism existed to deduplicate worker jobs.
- Graphile Worker is responsible for emitting background jobs (`notify_announcement`) that fan out the actual notification payloads.

## Decision
- Add a trigger-maintained `body_version` integer column on `announcement` that increments when the announcement body changes. This version becomes part of the dedupe key so only content changes retrigger notifications.
- Introduce a generic `notification` table with:
  - `tenant_id`, `user_id`, and `notification_type` columns to support multi-tenant scoping and multiple notification categories.
  - A `details` JSONB column to store type-specific metadata, such as the announcement ID and body version.
  - An optional `dedupe_key` with a partial unique index per tenant, user, and notification type to prevent repeated inserts for the same semantic event.
  - Row-level security policies restricting visibility to the owning user (while administrators retain full access).
- Update `queue_announcement_notifications` so that:
  - It verifies the announcement is currently visible, scheduled, and belongs to the active tenant.
  - It constructs a dedupe key `announcement:<id>:<body_version>`.
  - It resolves recipients based on announcement audiences (role- or cohort-based) and inserts notification rows using `ON CONFLICT DO NOTHING`.
  - It only enqueues the `notify_announcement` worker job when new notification rows were inserted, passing the announcement ID, body version, and new recipient list.

## Consequences
- First execution after a body change will enqueue `notify_announcement` and persist notifications for all recipients. Subsequent runs with the same `body_version` find conflicts and no job is enqueued, preventing duplicates.
- Introducing the `notification` table provides a reusable foundation for other notification types without additional schema changes.
- The dedupe behavior depends on the `body_version` trigger—manual body edits outside the trigger (e.g., bulk updates bypassing the trigger) could skip version increments, so operators must avoid bypassing application APIs.
- Storing per-user dedupe entries increases table size proportionally to the number of notifications but keeps notification delivery idempotent.

## Trade-offs
- Using a JSONB `details` field trades strict schema enforcement for flexibility; future consumers must validate payload structure in application code.
- Deduplication occurs at the per-user level. If a worker job fails mid-flight after inserting notification rows but before enqueuing downstream delivery, the job will not re-run automatically because the dedupe key already exists. The current implementation assumes the Graphile Worker job enqueue succeeds atomically with the insert.
- Trigger-based versioning only tracks body changes. If future requirements treat other fields (e.g., title) as content changes, the trigger must evolve accordingly.

## Future Improvements
- Wrap the notification insert and job enqueue in a PostgreSQL transaction that also schedules the worker job via `graphile_worker.add_job` to guarantee atomicity. Alternatively, use Graphile Worker's advisory locks or job-key dedupe features to align the database state with job dispatch.
- Consider storing a separate `content_hash` derived from the announcement payload to guard against manual updates that bypass the trigger or to make dedupe invariant to whitespace or formatting differences.
- Expose helper functions (e.g., `app_private.enqueue_notification(...)`) so other notification types can reuse the dedupe and RLS patterns without duplicating SQL boilerplate.
- Add automated tests (either SQL unit tests or integration tests in the worker layer) that simulate successive `queue_announcement_notifications` executions with and without body changes to verify the dedupe behavior over time.
- If notification volume grows, partition the `notification` table by tenant or time to keep index sizes manageable and improve query performance.

## References
- Graphile Worker documentation: https://graphile-worker.readthedocs.io/
- Existing tenant notification logic in `migrations/current/1-current.sql`
