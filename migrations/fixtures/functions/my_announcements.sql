create or replace function public.my_announcements(
  archive boolean default false,
  order_by_updated boolean default false
) returns setof public.upozorneni
    language sql stable
    as $$
  select upozorneni.* from upozorneni
  where is_visible = not archive and sticky = false
    and (scheduled_since is null or scheduled_since <= now())
    and (scheduled_until is null or scheduled_until >= now())
  order by
    case when order_by_updated then up_timestamp else up_timestamp_add end desc,
    up_timestamp_add desc;
$$;

grant all on function public.my_announcements(boolean, boolean) to anonymous;
