create or replace function public.title_videos() returns setof video as $$
  select * from video where v_id in (
    select pa_value::bigint from parameters where pa_name in (
      'title_video1', 'title_video2', 'title_video3', 'title_video4'
    )
  );
$$ language sql stable;
