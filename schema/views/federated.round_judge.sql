CREATE VIEW federated.round_judge AS
 SELECT DISTINCT round_id,
    judge_id
   FROM federated.judge_score;

GRANT SELECT ON TABLE federated.round_judge TO anonymous;
