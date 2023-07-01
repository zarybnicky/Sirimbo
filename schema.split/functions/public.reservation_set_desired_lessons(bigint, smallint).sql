CREATE FUNCTION public.reservation_set_desired_lessons(reservation_id bigint, lesson_count smallint, OUT reservation public.nabidka) RETURNS public.nabidka
    LANGUAGE plpgsql STRICT SECURITY DEFINER
    AS $$
begin
  select * into reservation from nabidka where n_id = reservation_id;

  if lesson_count = 0 then
    delete from nabidka_item where ni_id_rodic = reservation_id and ni_partner in (select * from current_couple_ids());
    return;
  end if;

  if lesson_count > (nabidka_my_lessons(reservation) + nabidka_free_lessons(reservation)) then
    select (nabidka_my_lessons(reservation) + nabidka_free_lessons(reservation))::smallint into lesson_count;
  end if;
  if reservation.n_max_pocet_hod > 0 and lesson_count > reservation.n_max_pocet_hod then
    select reservation.n_max_pocet_hod into lesson_count;
  end if;

  INSERT INTO nabidka_item
    (ni_id_rodic, ni_partner, ni_pocet_hod)
  values
    (reservation_id, (select current_couple_ids() limit 1), lesson_count)
  ON CONFLICT (ni_id_rodic, ni_partner)
  DO UPDATE SET ni_pocet_hod = lesson_count;

  select * into reservation from nabidka where n_id = reservation_id;
end;
$$;

GRANT ALL ON FUNCTION public.reservation_set_desired_lessons(reservation_id bigint, lesson_count smallint, OUT reservation public.nabidka) TO member;


