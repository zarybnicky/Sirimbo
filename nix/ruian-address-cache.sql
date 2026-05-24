SET preserve_insertion_order = false;

CREATE OR REPLACE TEMP MACRO normalize_ruian_text(value) AS
  trim(regexp_replace(lower(strip_accents(value)), '[^a-z0-9]+', ' ', 'g'));

CREATE OR REPLACE TEMP TABLE ruian_address_point_import AS
SELECT
  "Kód ADM"::INTEGER AS code,
  "PSČ"::INTEGER AS postal_code,
  "Číslo domovní"::INTEGER AS house_number,
  coalesce("Číslo orientační", 0)::INTEGER AS orientation_number,
  normalize_ruian_text(coalesce("Znak čísla orientačního", '')) AS orientation_suffix_norm,
  normalize_ruian_text(coalesce("Název ulice", '')) AS street_norm,
  normalize_ruian_text(coalesce("Název části obce", '')) AS part_norm,
  normalize_ruian_text(coalesce("Název obce", '')) AS municipality_norm,
  "Souřadnice X"::FLOAT AS x,
  "Souřadnice Y"::FLOAT AS y
FROM read_csv_auto(getenv('RUIAN_CSV_GLOB'), delim=';')
WHERE "Kód ADM" IS NOT NULL
  AND "PSČ" IS NOT NULL
  AND "Číslo domovní" IS NOT NULL
  AND "Souřadnice X" IS NOT NULL
  AND "Souřadnice Y" IS NOT NULL;

CREATE OR REPLACE TABLE ruian_address_point AS
SELECT
  code,
  postal_code,
  house_number,
  orientation_number,
  orientation_suffix_norm,
  street_norm,
  x,
  y
FROM ruian_address_point_import
ORDER BY postal_code, house_number, street_norm, code;

CREATE OR REPLACE TABLE ruian_postal_city_centroid AS
WITH places AS (
  SELECT postal_code, municipality_norm AS place_norm, x, y
  FROM ruian_address_point_import
  WHERE municipality_norm <> ''

  UNION ALL

  SELECT postal_code, part_norm AS place_norm, x, y
  FROM ruian_address_point_import
  WHERE part_norm <> ''
    AND part_norm <> municipality_norm
)
SELECT
  postal_code,
  place_norm,
  avg(x)::FLOAT AS x,
  avg(y)::FLOAT AS y,
  count(*)::INTEGER AS points
FROM places
GROUP BY postal_code, place_norm
ORDER BY postal_code, place_norm;

CREATE OR REPLACE TABLE ruian_postal_centroid AS
SELECT
  postal_code,
  avg(x)::FLOAT AS x,
  avg(y)::FLOAT AS y,
  count(*)::INTEGER AS points
FROM ruian_address_point
GROUP BY postal_code
ORDER BY postal_code;

CREATE OR REPLACE TABLE ruian_city_centroid AS
WITH places AS (
  SELECT municipality_norm AS place_norm, x, y
  FROM ruian_address_point_import
  WHERE municipality_norm <> ''

  UNION ALL

  SELECT part_norm AS place_norm, x, y
  FROM ruian_address_point_import
  WHERE part_norm <> ''
    AND part_norm <> municipality_norm
)
SELECT
  place_norm,
  avg(x)::FLOAT AS x,
  avg(y)::FLOAT AS y,
  count(*)::INTEGER AS points
FROM places
GROUP BY place_norm
ORDER BY place_norm;

CHECKPOINT;
