
do $$ begin
  if not exists (select 1 from information_schema.tables
    where table_schema = 'app_private' and table_name = 'crm_prospect'
  ) then

    create type app_private.crm_cohort as enum (
      'dancer',
      'hobbyist',
      'showdance',
      'free-lesson',
      'contact-me-later'
    );

    create type public.prospect_data as (
      name text,
      surname text,
      email text,
      phone text,
      yearofbirth text
    );

    create table app_private.crm_prospect (
      id serial primary key,
      cohort app_private.crm_cohort,
      data prospect_data,
      created_at timestamptz not null default now(),
      updated_at timestamptz not null default now()
    );
    create trigger _100_timestamps before insert or update on app_private.crm_prospect
      for each row execute procedure app_private.tg__timestamps();
    comment on table app_private.crm_prospect is E'@omit create';

    create table app_private.crm_activity (
      id serial primary key,
      prospect integer references app_private.crm_prospect (id),
      origin text not null,
      note text null,
      created_at timestamptz not null default now(),
      updated_at timestamptz not null default now()
    );
    create trigger _100_timestamps before insert or update on app_private.crm_activity
      for each row execute procedure app_private.tg__timestamps();
    comment on table app_private.crm_activity is E'@omit delete';
  end if;
end $$;

create or replace function public.prospect_form_dancer(
  cohort app_private.crm_cohort, prospect_data prospect_data, origin text, note text
) returns void as $$
declare
  prospect app_private.crm_prospect;
begin
  select * from app_private.crm_prospect where (data).email=prospect_data.email or (data).phone=prospect_data.phone into prospect;
  if prospect is null then
    insert into app_private.crm_prospect (cohort, data) values (cohort, prospect_data) returning * into prospect;
  end if;

  insert into app_private.crm_activity (prospect, origin, note) values (prospect.id, origin, note);
end;
$$ language plpgsql strict volatile security definer;
select plpgsql_check_function('public.prospect_form_dancer');

drop function active_prospects();
create or replace function active_prospects() returns table (
  id bigint, data prospect_data, cohort app_private.crm_cohort, last_activity timestamptz
) AS $$
  SELECT crm_prospect.id, crm_prospect.data, crm_prospect.cohort, crm_prospect.updated_at as last_activity
  FROM app_private.crm_prospect
  ORDER BY crm_prospect.updated_at DESC
$$ language sql stable security definer;

GRANT ALL ON FUNCTION public.active_prospects TO administrator;
