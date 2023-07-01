CREATE FUNCTION public.cancel_lesson(lesson_id bigint) RETURNS SETOF public.rozpis_item
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  schedule rozpis;
  lesson rozpis_item;
begin
  select * into lesson from rozpis_item where ri_id=lesson_id;
  select * into schedule from rozpis where r_id=lesson.ri_id_rodic;

  if schedule is null or lesson is null then
    raise exception 'ITEM_NOT_FOUND' using errcode = '28000';
  end if;

  if schedule.r_lock or lesson.ri_lock then
    raise exception 'ITEM_LOCKED' using errcode = '42501';
  end if;

  return query update rozpis_item set ri_partner = null where ri_id = lesson_id
    returning *;
end;
$$;

GRANT ALL ON FUNCTION public.cancel_lesson(lesson_id bigint) TO member;


