update crawler.frontier set process_status = 'pending'
where fetch_status in ('ok', 'pending');

select graphile_worker.complete_jobs(array_agg(id)) from graphile_worker.jobs where task_identifier in ('frontier_schedule', 'frontier_process');

TRUNCATE TABLE federated.federation;

INSERT INTO federated.federation (code, name)
VALUES ('wdsf', 'World DanceSport Federation'),
       ('csts', 'Český svaz tanečního sportu'),
       ('szts', 'Slovenský zväz tanečných športov');


TRUNCATE TABLE federated.dance;

INSERT INTO federated.dance (code, name, discipline) VALUES
  ('SW',  'Waltz',            'standard'),
  ('TG',  'Tango',            'standard'),
  ('VW',  'Viennese Waltz',   'standard'),
  ('SF',  'Slow Foxtrot',     'standard'),
  ('QS',  'Quickstep',        'standard'),
  ('SA',  'Samba',            'latin'),
  ('CH',  'Cha Cha',          'latin'),
  ('RU',  'Rumba',            'latin'),
  ('PD',  'Paso Doble',       'latin'),
  ('JI',  'Jive',             'latin'),
  ('PK',  'Polka',            'latin'),
  ('BA',  'Bachata',          'caribbean'),
  ('ME',  'Merengue',         'caribbean'),
  ('SL',  'Salsa',            'caribbean'),
  ('KRB', 'Caribbean',        'caribbean'),
  ('OT',  'Other',            'other');

select graphile_worker.add_job('frontier_process', json_build_object('isFullRebuild', true));
