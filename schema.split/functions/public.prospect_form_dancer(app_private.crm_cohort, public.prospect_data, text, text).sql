CREATE FUNCTION public.prospect_form_dancer(cohort app_private.crm_cohort, prospect_data public.prospect_data, origin text, note text) RETURNS void
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  prospect app_private.crm_prospect;
begin
  select * from app_private.crm_prospect where (data).email=prospect_data.email or (data).phone=prospect_data.phone into prospect;
  if prospect is null then
    insert into app_private.crm_prospect (cohort, data) values (cohort, prospect_data) returning * into prospect;
  end if;

  insert into app_private.crm_activity (prospect, origin, note) values (prospect.id, origin, note);
end;
$$;



