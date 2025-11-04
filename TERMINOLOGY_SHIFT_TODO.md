# Terminology shift to-do list

## Database table renames

For each table rename below:

1. Create a view using the target name that selects from the existing table (e.g., `create view financial_account as select * from account`) so new code can point at the view while legacy clients keep using the table. Verify in development that PostGraphile still exposes readable and writable operations against the view (simple `select *` views remain updatable).
2. Deploy application/worker/frontend changes against the new name (the view) and cut over runtime traffic.
3. Drop the compatibility view and rename the original table to the target name with `alter table account rename to financial_account`, updating sequences, indexes, policies, triggers, and foreign keys once clients no longer depend on the legacy table name.

- [ ] Rename `account` to `financial_account` (update sequences, primary key/index names, RLS policies, and references in functions and views).
- [ ] Rename `cohort_subscription` to `cohort_billing_plan` to separate enrollment vs. billing concepts (update dependent functions such as `create_missing_cohort_subscription_payments`).
- [ ] Rename `event_trainer` to `event_series_trainer` to clarify event-wide assignments and adjust dependent policies/triggers.
- [ ] Rename `event_instance_trainer` to `session_trainer` and update all foreign keys, triggers, and functions referencing the old name.
- [ ] Rename `aktuality` to `news_article` including all FK constraints (`aktuality_at_*`) and triggers.
- [ ] Rename `dokumenty` to `document_library_item` with matching FK/index renames.
- [ ] Rename `galerie_dir` to `gallery_folder` and cascade the rename to related indexes and FK constraints.
- [ ] Rename `galerie_foto` to `gallery_photo` and update references from `news_article`, `document_library_item`, and other dependents.
- [ ] Rename `upozorneni` to `announcement` and `upozorneni_skupiny` to `announcement_cohort_color`.
- [ ] Rename all legacy finance tables prefixed with `platby_` to English equivalents (`legacy_payment_category`, `legacy_payment_category_group`, `legacy_payment_group`, `legacy_payment_group_assignment`, `legacy_payment_item`, `legacy_payment_raw`) to remove Czech labels throughout the accounting layer.
- [ ] Review materialized views (`scoreboard`, `app_private.auth_details`, etc.) and update definitions once table renames are in place.

## Column and enum renames
- [ ] Replace Czech column names in `users` (e.g., `u_jmeno`, `u_prijmeni`, `u_pass`) with English equivalents while keeping legacy columns accessible via views if necessary for the transition.
- [ ] Rename Czech column prefixes in `aktuality`, `dokumenty`, `galerie_%`, and `upozorneni%` to descriptive English names; update triggers, generated columns, and policies accordingly.
- [ ] Review finance tables for ambiguous column names (`account_id`, `account_id_original`, etc.) and align them with the new table names to avoid confusion with authentication accounts.
- [ ] Audit enums/domains for Czech or overloaded terms (e.g., `event_type`, `transaction_source`) and document any additional renames required for clarity.

## Application layer follow-ups
- [ ] Update backend TypeScript, worker tasks, and SQL functions to reference the new table and column names.
- [ ] Regenerate GraphQL schema/codegen artifacts only after database renames are complete, then update frontend/worker GraphQL documents to the new identifiers.
- [ ] Adjust data loaders, URQL queries, and UI copy to match the renamed entities and columns.

## Documentation & communication
- [ ] Publish a migration playbook covering the table/column rename rollout (downtime expectations, verification steps, rollback plan).
- [ ] Update developer documentation to explain the distinction between authentication accounts and financial accounts after the rename.
- [ ] Refresh scheduling and finance SOPs to reflect the new trainer and billing terminology.
- [ ] Communicate the Czech-to-English renames to support and operations teams so they can update external-facing materials.

## Verification
- [ ] Run Graphile Migrate migrations for each rename in `migrations/current/1-current.sql`, promote, and verify against a staging database snapshot.
- [ ] Execute backend and frontend test suites plus schema checks after each batch of renames.
- [ ] Re-run `python schema/split.py < schema.sql` to regenerate the split schema once the canonical dump reflects the new names.
