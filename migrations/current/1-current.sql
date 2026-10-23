comment on table person is '@behavior -insert
@ref federationPeople via:public.person_federation_link;federated.person
@ref federationCategoryProgresses via:public.person_federation_link;federated.person;federated.competitor_component;federated.competitor;federated.competitor_category_progress
@ref federationRanklistEntries via:public.person_federation_link;federated.person;federated.competitor_component;federated.competitor;federated.ranklist_entry
@ref federationCompetitions via:public.person_federation_link;federated.person;federated.competitor_component;federated.competitor;federated.competition_participation;federated.competition';
comment on table couple is '@behavior +list -connection -query:resource:list -query:resource:connection
@ref federationCompetitors via:public.couple_federation_link;federated.competitor
@ref federationRanklistEntries via:public.couple_federation_link;federated.competitor;federated.ranklist_entry
@ref federationCompetitions via:public.couple_federation_link;federated.competitor;federated.competition_participation;federated.competition behavior:"+connection -list"';

comment on table users is '@behavior -insert -update -delete';
comment on table payment is '@behavior +list -connection -insert -query:resource:list -query:resource:connection';
comment on table event is '@behavior +list -connection -insert';
comment on table tenant_trainer is '@behavior +list +connection -query:resource:list -query:resource:connection';
comment on table event_lesson_request is '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';
comment on table event_registration is '@behavior +list +connection -insert -update -delete';
comment on table access_credential is '@behavior +list -connection -delete';
comment on table account is '@behavior +list -connection -insert -update -delete -query:resource:single';
comment on table posting is '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';
comment on table transaction is '@behavior -insert -update';
comment on table announcement is '@behavior -insert';
comment on table cohort is '@behavior +list -connection';
comment on table cohort_subscription is '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';
comment on table event_trainer is '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';
comment on table tenant is '@behavior +list -connection -insert -delete -singularRelation:resource:single -query:resource:connection';
comment on table payment_debtor is '@behavior +list -connection -insert -update -delete';
comment on table membership_application is '@behavior +list -connection';
comment on table tenant_administrator is '@behavior +list +connection -query:resource:list -query:resource:connection';
comment on table tenant_membership is '@behavior +list +connection -query:resource:list -query:resource:connection';
comment on table tenant_settings is '@behavior -insert -delete -query:resource:list -query:resource:connection -singularRelation:resource:list';
comment on table aktuality is '@behavior -insert -update -query:resource:list';
comment on table location is '@behavior +list -connection -query:resource:list -query:resource:connection -queryField:resource:connection';
comment on table access_event is '@behavior +list -connection -insert -update -delete';
comment on table accounting_period is '@behavior -*';
comment on table dokumenty is '@behavior -*';
comment on table otp_token is '@behavior -*';
comment on table announcement_audience is '@behavior -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';
comment on table cohort_group is '@behavior -insert -update -delete';
comment on table cohort_membership is '@behavior +list -connection -query:resource:list -query:resource:connection';
comment on table event_target_cohort is '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';
comment on table file is '@behavior -insert -update -delete';
comment on table form_responses is '@behavior -insert -update -delete';
comment on table location_image is '@behavior +list -connection -insert -update -delete -query:resource:connection -query:resource:list';
comment on table payment_recipient is '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection';
comment on table person_invitation is '@behavior +list -connection -update -query:resource:list -query:resource:connection';
comment on table user_proxy is '@behavior +list -connection -query:resource:list -query:resource:connection';
comment on table scoreboard_manual_adjustment is '@behavior +list -connection -query:resource:list -query:resource:connection';
comment on table security_event is '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';

comment on function create_missing_cohort_subscription_payments is '@behavior -*';
comment on function create_next_cohort_subscription_payment is '@behavior -*';
comment on function current_tenant_id is '@behavior -*';
comment on function current_user_id is '@behavior -*';
comment on function verify_function is '@behavior -*';
comment on function payment_debtor_is_unpaid is '@behavior +filter +filterProc +filterBy';

comment on column access_credential.valid_range is '@behavior -*';
comment on column cohort_membership.active_range is '@behavior -*';
comment on column couple.legacy_pary_id is '@behavior -*';
comment on column couple.active_range is '@behavior -*';
comment on column event.share_token is '@behavior -*';
comment on column event_registration.created_by is '@behavior -*';
comment on column tenant_administrator.active_range is '@behavior -*';
comment on column tenant_membership.active_range is '@behavior -*';
comment on column tenant_trainer.active_range is '@behavior -*';
comment on column person_invitation.event_id is '@behavior -*';
comment on column person.legacy_user_id is '@behavior -*';
comment on column person.search_name is '@behavior -*';
comment on column user_proxy.active_range is '@behavior -*';
comment on column users.u_pass is '@behavior -*';

comment on view current_cohort_membership is '@behavior -*';
comment on view current_tenant_administrator is '@behavior -*';
comment on view current_tenant_membership is '@behavior -*';
comment on view current_tenant_trainer is '@behavior -*';
comment on materialized view account_balances is '@behavior -*';

comment on constraint event_registration_created_by_fkey on event_registration is '@behavior -*';
comment on constraint person_invitation_event_id_fkey on person_invitation is '@behavior -*';

--!include functions/competition_reports.sql
--!include functions/create_event_payment.sql
--!include functions/current_account_ids.sql
--!include functions/current_person_ids.sql
--!include functions/event_approx_price.sql
--!include functions/event_instances_for_range.sql
--!include functions/event_overlaps_reports.sql
--!include functions/event_trainers.sql
--!include functions/get_current_user.sql
--!include functions/payment_debtor_price.sql
--!include functions/person_account.sql
--!include functions/person_couples.sql
--!include functions/person_federation_candidates.sql
--!include functions/person_with_invitation.sql
--!include functions/resolve_payment_with_credit.sql
--!include functions/scoreboard.sql
--!include functions/tenant_account.sql
--!include functions/tenant_couples.sql
--!include functions/trainer_group_attendance_completion.sql
--!include functions/log_in_as.sql
--!include functions/refresh_jwt.sql
--!include functions/current_claims.sql
--!include functions/submit_form.sql
--!include functions/tg_cohort_membership__on_status.sql
--!include functions/event_registration_info.sql
--!include functions/event_remaining_x.sql
