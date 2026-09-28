# Migrations scratchpad manual for AI contributors

This guide covers changes under `migrations/current/` and `migrations/fixtures/`, including migration promotion with Graphile Migrate.

## Must dos
- **Rebuild new, empty tables during development.** For tables introduced by the current work, edit the original definition instead of accumulating `alter table` statements. You can drop and recreate these tables without additional approval when they contain no data to preserve. For existing tables or columns, require explicit task authorization before dropping them. Recreating a table does not restore its data.
- **Refresh supporting metadata.** Whenever you define or change a table, function, or trigger, include the accompanying `comment on ...` statements (for Graphile hints) and explicit `grant`/`revoke` statements to keep permissions consistent.
- **Use existing helpers.** Reuse helpers for policy resets, background jobs, and watcher notifications. Use `graphile_worker.add_job(...)` with an existing task from `worker/tasks/`. Use `postgraphile_watch.notify_watchers_*()` for watcher notifications.
- **Document complex operations inline.** Add concise SQL comments explaining non-obvious sequences, especially when coordinating multiple triggers/functions.

### Example patterns from committed migrations
- **New, empty table rebuilds.** Use this pattern only for tables introduced by the current work with no data to preserve:
  ```sql
  drop table if exists event_external_registration;
  create table if not exists event_external_registration (
    ...
  );
  ```
- **Guarded column changes.** Use existence guards so column additions and authorized removals succeed on reruns. These guards do not protect data or dependent objects:
  ```sql
  alter table attachment add column if not exists thumbhash text null;
  ```
- **Adding enum values safely.** Use an existence check before `alter type ... add value`, as seen in `migrations/committed/000041.sql`:
  ```sql
  do $$
  begin
    if not exists (
      select 1 from pg_catalog.pg_enum e
      join pg_catalog.pg_type t on t.oid = e.enumtypid
      where t.typname = 'attendance_type' and e.enumlabel = 'cancelled'
    ) then
      alter type attendance_type add value 'cancelled';
    end if;
  end;
  $$;
  ```
- **Removing enum values.** When PostgreSQL cannot drop a label directly, rename and recreate the type like `migrations/committed/000053.sql` does:
  ```sql
  alter type attendance_type rename to attendance_type_old;
  create type attendance_type as enum ('unknown', 'attended', 'not-excused', 'cancelled');
  alter table event_attendance alter column status type attendance_type using status::text::attendance_type;
  drop type attendance_type_old;
  ```
- **Renames and old readers.** Prefer a rename plus a generated column under the old name when compatibility requires only reads. The generated column derives its value from the renamed column. Old writes to that generated column are not supported. For table renames, consider a compatibility view with the required permissions and row-level security behavior. Make sure that old GraphQL queries still work. Guard renames so the current migration can run again. Add write synchronization only when the task requires old writers to remain supported.
- **Deployment order.** PostGraphile reloads the API schema after database changes. For order-dependent changes, describe the boundaries for separate commits and their deployment order. State when old columns or views can be removed. The maintainer creates the Git commits and deploys them in order. Do not add deployment scripts solely to enforce this sequence.
- **Resetting and recreating RLS policies.** Pair `app_private.drop_policies` with new policies and grants, following `migrations/committed/000052.sql`:
  ```sql
  select app_private.drop_policies('public.tenant_settings');
  create policy tenant_settings_select on public.tenant_settings for select to member using (...);
  grant select on public.tenant_settings to member;
  ```

## Local workflow
- Preserve unrelated SQL already in `1-current.sql`. Migration promotion includes the whole scratchpad, not just your changes. Leave promotion to the maintainer unless the task explicitly includes that work.
- Edit `1-current.sql` or a fixture under `migrations/fixtures/`.
- For each new or changed fixture, add its `--!include` directive to `1-current.sql`. Paths are relative to `migrations/fixtures/`, such as `--!include functions/upsert_location.sql`.
- Before running migrations, make sure that `DATABASE_URL` and `SHADOW_DATABASE_URL` identify local development databases.
- The Overmind `migrate` process runs `graphile-migrate watch` and can apply edits before a manual command runs. Do not start another watcher.
- After editing SQL, run `pnpm exec graphile-migrate current --forceActions`, even when the watcher is running. This applies pending SQL and runs the `.gmrc` tests, including SQL function checks and pgTAP assertions.
- Require a zero exit status and successful test output before reporting success. If the command fails, use its error output to diagnose the failure. An unchanged-SQL message is normal when the watcher already applied the SQL. The `--forceActions` flag still runs the tests. Do not change SQL merely to force the watcher to run again.
- After permission changes, test both allowed and denied access, including access from another tenant. Add or update the relevant tests in `migrations/test/` for high-exposure changes.
- Test repeatability with `pnpm exec graphile-migrate run migrations/current/1-current.sql` against the same local database. Then run `pnpm exec graphile-migrate current --forceActions` again for the tests. The `current` and `watch` commands skip unchanged SQL, so invoking them twice does not test repeatability.
- When the migration is ready for promotion, run `pnpm exec graphile-migrate commit`. This creates a committed migration file and refreshes the schema dump through the configured hooks. It does not create a Git commit or change the Git index.
- Do not hand-edit files under `migrations/committed/`. Put later corrections in the current migration.

## Don't dos
- **Do not write non-repeatable DDL.** Avoid bare `insert`, `update`, or `delete` statements that would error or duplicate data on reruns; always include conflict handling or checks.
- **Do not treat existence guards as data protection.** Apply the new-table exception and authorization rules above. Preserve existing data and required readers across renames.
- **Do not disable RLS or grants implicitly.** Every change must leave row-level security and permissions in a valid state—if you drop and recreate a table or function, restate the grants and policies within the same migration.
- **Do not remove the `--!` metadata headers Graphile Migrate expects.** Preserve include directives or file headers already present in `1-current.sql` when editing.
