# Migrations scratchpad manual for AI contributors

This guide governs all files under `migrations/current/`. Follow it before promoting a migration with Graphile Migrate.

## Must dos
- **Only drop objects with explicit user approval.** Require explicit instructions from the task author before emitting any `drop table` or `alter table ... drop column`. When authorized, restrict the drop to objects you are recreating in the same migration so no production data is lost. For structural renames, follow the multi-step pattern (new table + view bridge) described below instead of dropping the live table.
- **Re-create row-level security policies safely.** Call `select app_private.drop_policies('schema.table');` before redefining policies so reruns do not fail, then issue fresh `create policy` and `grant` statements to restore access controls.
- **Verify PL/pgSQL routines after defining them.** After `create or replace function ... language plpgsql`, add `select verify_function('function_name'[, 'table_name']);` so compile errors surface at migration time.
- **Refresh supporting metadata.** Whenever you define or change a table, function, or trigger, include the accompanying `comment on ...` statements (for Graphile hints) and explicit `grant`/`revoke` statements to keep permissions consistent.
- **Prefer helper APIs for background jobs.** Use existing helpers like `graphile_worker.add_job(...)` and `postgraphile_watch.notify_watchers_*()` instead of reimplementing notification logic.
- **Document complex operations inline.** Add concise SQL comments explaining non-obvious sequences, especially when coordinating multiple triggers/functions.

### Example patterns from committed migrations
- **Idempotent table rebuilds.** Follow the guarded drop + create pattern from `migrations/committed/000047.sql` when you need to recreate a table:
  ```sql
  drop table if exists event_external_registration;
  create table if not exists event_external_registration (
    ...
  );
  ```
- **Guarded column changes.** Mirror `migrations/committed/000016.sql` and `000027.sql` by wrapping `ALTER TABLE` column additions/removals with `if not exists` / `if exists` so reruns succeed:
  ```sql
  alter table attachment add column if not exists thumbhash text null;
  alter table skupiny drop column if exists cohort_group cascade;
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
- **Renaming tables without data loss.** Take the three-step approach used across historical migrations when renaming: create the replacement table under the new name, populate it from the old structure with conflict-safe `insert`/`update` (see the copy pattern in `migrations/committed/000037.sql`), and expose a compatibility view under the old name until application code switches over. Only drop the compatibility view after verifying no consumers require it.
- **Resetting and recreating RLS policies.** Pair `app_private.drop_policies` with new policies and grants, following `migrations/committed/000052.sql`:
  ```sql
  select app_private.drop_policies('public.tenant_settings');
  create policy tenant_settings_select on public.tenant_settings for select to member using (...);
  grant select on public.tenant_settings to member;
  ```
- **Verifying functions after updates.** Immediately call `verify_function` after `create or replace function`, mirroring `migrations/committed/000053.sql`:
  ```sql
  create or replace function event_instance_approx_price(...) returns numeric as $$
  begin
    ...
  end;
  $$ language plpgsql stable;
  select verify_function('event_instance_approx_price');
  ```
- **Scheduling background jobs.** Delegate to helpers such as `graphile_worker.add_job`, as demonstrated in `migrations/committed/000052.sql`:
  ```sql
  perform graphile_worker.add_job('refresh_auth_details', job_key := 'refresh_auth_details');
  ```

## Don't dos
- **Do not write non-repeatable DDL.** Avoid bare `insert`, `update`, or `delete` statements that would error or duplicate data on reruns; always include conflict handling or checks.
- **Do not drop or rename live objects without safeguards or direction.** Never emit unconditional `drop table`, `drop function`, or `alter table ... drop column`; wrap them in `if exists`/`if not exists` clauses. Only perform table or column drops when the user has explicitly requested them, and favor the create-copy-view pattern rather than dropping the original table for renames.
- **Do not bypass existing helpers.** If a helper function already encapsulates logic (e.g., for policy resets, trigger verification, or auth refresh jobs), call it rather than duplicating its behavior.
- **Do not disable RLS or grants implicitly.** Every change must leave row-level security and permissions in a valid state—if you drop and recreate a table or function, restate the grants and policies within the same migration.
- **Do not remove the `--!` metadata headers Graphile Migrate expects.** Preserve include directives or file headers already present in `1-current.sql` when editing.
