--! include functions/event_instance_registration_last_attended.sql
--! include functions/event_instance_approx_price.sql
--! include functions/set_event_instance_registration.sql
--! include functions/set_lesson_demand.sql

drop function if exists federated.competition_sankey_links;

create or replace function public.current_claims() returns jsonb
    language sql stable security definer
as $$
  select to_jsonb(app_private.create_jwt_token(users))
    - 'exp'
    - 'is_member'
    - 'is_trainer'
    - 'is_admin'
  from users
  where id = nullif(current_setting('jwt.claims.user_id', true), '')::bigint;
$$;

grant all on function public.current_claims() to anonymous;

comment on function competition_brief is '@omit';
comment on function competition_report is '@omit';
