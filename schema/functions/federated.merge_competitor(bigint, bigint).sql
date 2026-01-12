CREATE FUNCTION federated.merge_competitor(in_old bigint, in_keep bigint) RETURNS void
    LANGUAGE plpgsql
    SET search_path TO 'federated', 'pg_temp'
    AS $$
BEGIN
  IF in_old IS NULL OR in_keep IS NULL OR in_old = in_keep THEN
    RETURN;
  END IF;

  -- Tables where UPDATE is not safe (competitor_id part of PK/unique)
  INSERT INTO federated.competition_entry (competition_id, competitor_id, cancelled, created_at)
  SELECT competition_id, in_keep, cancelled, created_at
  FROM federated.competition_entry WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  INSERT INTO federated.competition_round_result (round_id, competitor_id, overall_ranking, overall_ranking_to, qualified_next, overall_score)
  SELECT round_id, in_keep, overall_ranking, overall_ranking_to, qualified_next, overall_score
  FROM federated.competition_round_result WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  INSERT INTO federated.competition_result (competition_id, competitor_id, start_number, ranking, ranking_to, point_gain, final_gain)
  SELECT competition_id, in_keep, start_number, ranking, ranking_to, point_gain, final_gain
  FROM federated.competition_result WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  INSERT INTO federated.judge_score (federation, event_date, event_id, competition_id, category_id, round_id, dance_code, judge_id, competitor_id, component, score, raw_score)
  SELECT federation, event_date, event_id, competition_id, category_id, round_id, dance_code, judge_id, in_keep, component, score, raw_score
  FROM federated.judge_score WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  INSERT INTO federated.ranklist_entry (snapshot_id, competitor_id, ranking, ranking_to, points)
  SELECT snapshot_id, in_keep, ranking, ranking_to, points
  FROM federated.ranklist_entry WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  INSERT INTO federated.competitor_category_progress (federation, competitor_id, category_id, points, domestic_finale, foreign_finale)
  SELECT federation, in_keep, category_id, points, domestic_finale, foreign_finale
  FROM federated.competitor_category_progress WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  INSERT INTO federated.competitor_club_affiliation (competitor_id, club_id, valid_from, valid_to)
  SELECT in_keep, club_id, valid_from, valid_to
  FROM federated.competitor_club_affiliation WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  INSERT INTO federated.federation_competitor (federation, external_id, competitor_id, age_group)
  SELECT federation, external_id, in_keep, age_group
  FROM federated.federation_competitor WHERE competitor_id = in_old
  ON CONFLICT DO NOTHING;

  DELETE FROM federated.competition_entry WHERE competitor_id = in_old;
  DELETE FROM federated.competition_round_result WHERE competitor_id = in_old;
  DELETE FROM federated.competition_result WHERE competitor_id = in_old;
  DELETE FROM federated.judge_score WHERE competitor_id = in_old;
  DELETE FROM federated.ranklist_entry WHERE competitor_id = in_old;
  DELETE FROM federated.competitor_category_progress WHERE competitor_id = in_old;
  DELETE FROM federated.competitor_club_affiliation WHERE competitor_id = in_old;
  DELETE FROM federated.federation_competitor WHERE competitor_id = in_old;

  DELETE FROM federated.competitor WHERE id = in_old;
END;
$$;
