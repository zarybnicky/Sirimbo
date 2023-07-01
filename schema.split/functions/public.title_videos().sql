CREATE FUNCTION public.title_videos() RETURNS SETOF app_private.video
    LANGUAGE sql STABLE
    AS $$
  select * from video where v_id in (
    select pa_value::bigint from parameters where pa_name in (
      'title_video1', 'title_video2', 'title_video3', 'title_video4'
    )
  );
$$;



