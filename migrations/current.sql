-- Write your migration here
grant all on function submit_form to anonymous;

select app_private.drop_policies('public.form_responses');
grant all on table form_responses to anonymous;
alter table form_responses enable row level security;
create policy admin_all on form_responses to administrator using (true) with check (true);
