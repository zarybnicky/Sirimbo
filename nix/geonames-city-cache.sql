SET preserve_insertion_order = false;

CREATE MACRO normalize_geonames_name(value) AS
  trim(regexp_replace(lower(strip_accents(value)), '[^\p{L}\p{N}]+', ' ', 'g'));

CREATE TEMP TABLE cities AS
SELECT * FROM read_csv(getenv('GEONAMES_CITIES'), delim='\t', header=false, quote='', columns={
  'id': 'INTEGER', 'name': 'VARCHAR', 'ascii_name': 'VARCHAR', 'aliases': 'VARCHAR',
  'lat': 'DOUBLE', 'lng': 'DOUBLE', 'feature_class': 'VARCHAR', 'feature_code': 'VARCHAR',
  'country': 'VARCHAR', 'cc2': 'VARCHAR', 'admin1': 'VARCHAR', 'admin2': 'VARCHAR',
  'admin3': 'VARCHAR', 'admin4': 'VARCHAR', 'population': 'BIGINT',
  'elevation': 'VARCHAR', 'dem': 'VARCHAR', 'timezone': 'VARCHAR', 'modified': 'DATE'
});

-- Settlements absent from cities500
INSERT INTO cities (id, name, ascii_name, aliases, lat, lng, country, admin1, admin2, population)
SELECT * FROM (VALUES
  (759985, 'Rusiec', 'Rusiec', 'Rusiec', 52.07792, 20.78845, 'PL', '78', '1421', 0),
  (3171709, 'Paestum', 'Paestum', 'Paestum,Pesto,Pestum,Poseidonia', 40.4223, 15.00731, 'IT', '04', 'SA', 150),
  (617629, 'Pănăşeşti', 'Panasesti', 'Panasesti', 47.15556, 28.52021, 'MD', '89', NULL, 0),
  (1707988, 'Lahug', 'Lahug', '', 10.33405, 123.89329, 'PH', '07', '21', 0),
  (733702, 'Albena', 'Albena', 'Albena,Албена', 43.38333, 28.08333, 'BG', '40', 'DOB03', 23),
  (2743947, 'Zevenbergen', 'Zevenbergen', 'Zevenbergen', 51.645, 4.60417, 'NL', '06', '1709', 0),
  (2747191, 'Slagharen', 'Slagharen', '', 52.62917, 6.55556, 'NL', '15', '0160', 0),
  (7729796, 'Neupré', 'Neupre', '', 50.54387, 5.48012, 'BE', 'WAL', 'WLG', 0),
  (2902519, 'Höfingen', 'Hoefingen', 'Hofingen,Höfingen', 48.8179, 9.01686, 'DE', '01', '081', 0)
) AS extra(id, name, ascii_name, aliases, lat, lng, country, admin1, admin2, population)
WHERE extra.id NOT IN (SELECT id FROM cities);

CREATE TABLE geonames_city AS
SELECT id, country, name, lat, lng, population, admin1, admin2 FROM cities;

CREATE TABLE geonames_city_name AS
WITH names AS (
  SELECT country, id, unnest([name, ascii_name]) AS alias, 0 AS priority FROM cities
  UNION ALL
  SELECT country, id, unnest(string_split(aliases, ',')), 1 FROM cities
  UNION ALL
  SELECT country, id, alias, 1 FROM (VALUES
    ('ES', 3125800, 'Platja d''Aro'),
    ('AT', 2765388, 'Schwechat/Vienna'),
    ('IT', 3172681, 'Montichiari Brescia'),
    ('BR', 3451190, 'Rio de Janerio'),
    ('IT', 3182297, 'Bassano Dell Grappa'),
    ('MT', 2562541, 'Malta'),
    ('IT', 6535205, 'Cossirano di Trenzano (BS)'),
    ('NO', 3150851, 'Oslo (Jessheim)'),
    ('RU', 524901, 'Moscow Region'),
    ('CN', 1795270, 'Shijazhuang'),
    ('MD', 617629, 'Straseni district, Panasest village'),
    ('JP', 1858088, 'Fukuoka Kurume City'),
    ('TR', 303831, 'Cappadocia'),
    ('PH', 1707988, 'Lahug , Cebu City'),
    ('HK', 1819729, 'Hong Kong, China'),
    ('HU', 3044310, 'szombatheley'),
    ('PT', 2732547, 'Vila Nova Famalicao'),
    ('RU', 520555, 'N. Novgorod'),
    ('RU', 520555, 'N.Novgorod'),
    ('RU', 520555, 'Nijniy Novgorod'),
    ('NL', 2759661, 'Aarnhem'),
    ('PL', 3097257, 'Jelena Gora'),
    ('LU', 2960596, 'Esch-Alzette'),
    ('AT', 2765388, 'Vienna-Schwechat'),
    ('KR', 1845136, 'Chun Cheon City'),
    ('KR', 1845136, 'Chun Cheon'),
    ('PL', 756135, 'Warzaw'),
    ('PL', 756135, 'Warzawa'),
    ('UA', 709930, 'Dniepropetrovsk'),
    ('ES', 3118212, 'Lloret'),
    ('FI', 649360, 'Lathi'),
    ('IT', 3182851, 'Aricca'),
    ('UA', 709717, 'Donetzk'),
    ('CZ', 3074967, 'Hradek Kralove'),
    ('UA', 7525990, 'Uzhnoukrainsk'),
    ('IT', 3164241, 'Villafranca Veronese'),
    ('BA', 3203521, 'Brcko District'),
    ('GB', 2653822, 'Cardif'),
    ('UA', 710719, 'Chemivtsi'),
    ('UA', 710719, 'Chernivtsy'),
    ('BA', 3191281, 'Sarajavo'),
    ('BY', 620127, 'Vitsbsk'),
    ('PL', 3080985, 'Zabre'),
    ('BY', 625144, 'Minks'),
    ('PT', 2267057, 'Lisbao'),
    ('RU', 554234, 'Kalingrad'),
    ('PL', 3102014, 'Bydgoszoz'),
    ('FR', 2995469, 'Marceille'),
    ('FR', 2995469, 'Marseile'),
    ('HU', 3049880, 'Kiskunhala'),
    ('IT', 3170752, 'Oieve Di Cento'),
    ('UA', 6930327, 'Slavytich'),
    ('GR', 258576, 'Larissa Town'),
    ('LU', 2960316, 'Luxembourgh'),
    ('IT', 8948704, 'Monteruscello'),
    ('PL', 757809, 'Sulejowka'),
    ('FR', 2994768, 'Meg&Egrave;Ve'),
    ('ES', 3125800, 'Platja de Aro'),
    ('KZ', 610529, 'Athyrau'),
    ('FI', 658225, 'Heslinki'),
    ('BG', 733702, 'Albeda'),
    ('PT', 2732544, 'Vila Nova Gaia'),
    ('UA', 712165, 'Belaya Tscerkov'),
    ('US', 4934664, 'Dedham Massachusetts'),
    ('SK', 3057124, 'Tyrnavia'),
    ('PL', 3082707, 'Sudeckie Perly')
  ) AS aliases(country, id, alias)
)
SELECT country, normalize_geonames_name(alias) AS name, id, min(priority) AS priority
FROM names WHERE normalize_geonames_name(alias) <> ''
GROUP BY country, name, id;
CREATE INDEX geonames_city_name_idx ON geonames_city_name(country, name);

CREATE TABLE geonames_country_name AS
WITH countries AS (
  SELECT * FROM read_csv(getenv('GEONAMES_COUNTRIES'), delim='\t', header=false, quote='',
    comment='#', all_varchar=true, null_padding=true)
), aliases AS (
  SELECT column00 AS country, unnest([column00, column01, column04]) AS name FROM countries
  UNION ALL
  SELECT * FROM (VALUES
    ('CN', 'China, People''s Republic of'), ('TW', 'Chinese Taipei'),
    ('GB', 'England'), ('GB', 'Wales'), ('GB', 'Scotland'),
    ('IE', 'Ireland, Republic of'), ('KR', 'Korea'), ('MK', 'Macedonia'),
    ('RU', 'Russian Federation'), ('TR', 'Türkiye'), ('CZ', 'Czech Republic'),
    ('NL', 'Netherlands'), ('MO', 'Macau')
  )
)
SELECT DISTINCT country, normalize_geonames_name(name) AS name FROM aliases;

CREATE TABLE geonames_area_name AS
WITH names AS (
  SELECT split_part(code, '.', 1) AS country, split_part(code, '.', 2) AS admin1,
    NULL::VARCHAR AS admin2, unnest([name, ascii_name, split_part(code, '.', 2)]) AS name
  FROM read_csv(getenv('GEONAMES_REGIONS'), delim='\t', header=false, quote='',
    columns={'code': 'VARCHAR', 'name': 'VARCHAR', 'ascii_name': 'VARCHAR', 'id': 'INTEGER'})
  UNION ALL
  SELECT country, admin1, admin2, admin2 FROM cities
)
SELECT DISTINCT country, admin1, admin2, normalize_geonames_name(name) AS name
FROM names WHERE name IS NOT NULL;

CHECKPOINT;
