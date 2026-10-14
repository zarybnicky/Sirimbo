create or replace function federated.resolve_age_group(source_name text) returns text
language sql stable strict as $$
  select code from federated.age_group
  where regexp_replace(lower(source_name), '[^a-z0-9]', '', 'g')
    = any(array_prepend(regexp_replace(lower(code), '[^a-z0-9]', '', 'g'), aliases));
$$;
comment on function federated.resolve_age_group(text) is '@behavior -*';
revoke all on function federated.resolve_age_group(text) from public;
grant execute on function federated.resolve_age_group(text) to anonymous;
