CREATE FUNCTION public.book_lesson(lesson_id bigint) RETURNS SETOF public.rozpis_item
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
declare
  schedule rozpis;
  lesson rozpis_item;
  couple_id bigint;
begin
  select * into lesson from rozpis_item where ri_id=lesson_id;
  select * into schedule from rozpis where r_id=lesson.ri_id_rodic;
  select * into couple_id from current_couple_ids() limit 1;

  if schedule is null or lesson is null then
    raise exception 'ITEM_NOT_FOUND' using errcode = '28000';
  end if;

  if schedule.r_lock or lesson.ri_lock then
    raise exception 'ITEM_LOCKED' using errcode = '42501';
  end if;

  return query update rozpis_item set ri_partner = couple_id where ri_id = lesson_id
    returning *;
end;
$$;

GRANT ALL ON FUNCTION public.book_lesson(lesson_id bigint) TO member;


