/* @name TouchIngestRecord */
update csts.ingest set checked_at = now()
where type = :type and url = :url and hash = :hash;

/* @name UpsertIngestRecord */
insert into csts.ingest (type, url, hash, payload)
values (:type, :url, :hash, :payload::jsonb)
on conflict (type, url, hash)
do update set payload = excluded.payload, checked_at = now();

/* @name LoadIngestRecord */
select hash, payload, created_at, checked_at
from csts.ingest
where type = :type and url = :url
order by checked_at desc, created_at desc
limit 1;

/*
@name DeleteIngestByUrls
@param urls -> (...)
*/
delete from csts.ingest where type = :type and url in :urls;

/* @name UpsertAthlete */
insert into csts.athlete (
  idt,
  name,
  age_category,
  sex,
  medical_checkup_expiration
)
values (:idt, :name, :age::text, :sex, :medicalCheckupExpiration::date)
on conflict (idt) do update set
  name = excluded.name,
  age_category = excluded.age_category,
  sex = excluded.sex,
  medical_checkup_expiration = excluded.medical_checkup_expiration,
  fetched_at = now();

/* @name UpsertCompetitorRanking */
insert into csts.competitor_ranking (
  competitor_id,
  discipline,
  ranking_points_age,
  ranking_age,
  competitor_age,
  series,
  competitors,
  class,
  points,
  domestic_finale_count,
  foreign_finale_count,
  ranklist_ranking,
  ranklist_points,
  athlete_idt,
  couple_id
)
values (
  :competitorId, :discipline, :rankingPointsAge, :rankingAge, :age, :series, :competitors, :class, :points,
  :domesticFinaleCount, :foreignFinaleCount, :ranklistRanking, :ranklistPoints, :athleteIdt, :coupleId
)
on conflict (competitor_id, discipline) do update set
  ranking_points_age = excluded.ranking_points_age,
  ranking_age = excluded.ranking_age,
  competitor_age = excluded.competitor_age,
  series = excluded.series,
  competitors = excluded.competitors,
  class = excluded.class,
  points = excluded.points,
  domestic_finale_count = excluded.domestic_finale_count,
  foreign_finale_count = excluded.foreign_finale_count,
  ranklist_ranking = excluded.ranklist_ranking,
  ranklist_points = excluded.ranklist_points,
  athlete_idt = excluded.athlete_idt,
  couple_id = excluded.couple_id;

/* @name UpsertAthleteRanking */
insert into csts.athlete_ranking (
  athlete_id,
  discipline,
  series,
  personal_class,
  personal_points,
  personal_domestic_finale_count,
  personal_foreign_finale_count
)
values (:athleteId, :discipline, :series, :personalClass, :personalPoints, :personalDomesticFinaleCount, :personalForeignFinaleCount)
on conflict (athlete_id, discipline, series) do update set
  personal_class = excluded.personal_class,
  personal_points = excluded.personal_points,
  personal_domestic_finale_count = excluded.personal_domestic_finale_count,
  personal_foreign_finale_count = excluded.personal_foreign_finale_count;

/* @name UpsertCouple */
insert into csts.couple (id, couple_idt, man_idt, woman_idt, formed_at)
select
  :competitorId,
  :coupleIdt,
  case when a1.sex = 'M' then a1.idt else a2.idt end as man_idt,
  case when a1.sex = 'F' then a1.idt else a2.idt end as woman_idt,
  :formedAt::timestamptz
from csts.athlete a1
join csts.athlete a2 on a2.idt = :partnerIdt
where a1.idt = :athleteIdt
  and ((a1.sex = 'M' and a2.sex = 'F') or (a1.sex = 'F' and a2.sex = 'M'))
on conflict (id) do update set
  couple_idt = excluded.couple_idt,
  man_idt    = excluded.man_idt,
  woman_idt  = excluded.woman_idt,
  formed_at  = excluded.formed_at
returning id;
