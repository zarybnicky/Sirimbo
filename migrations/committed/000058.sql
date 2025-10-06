--! Previous: sha1:a62e79890f2060636af8b38d578a46151ce5d5dd
--! Hash: sha1:2e32f51a85842cab7f681c2cfdacd1bbaea9fddd

--! split: 1-current.sql
drop table if exists upozorneni cascade;

COMMENT ON TABLE public.upozorneni_skupiny IS '@omit create,update,delete';

ALTER TABLE ONLY upozorneni_skupiny
    DROP CONSTRAINT if exists upozorneni_skupiny_ups_id_rodic_fkey,
    ADD CONSTRAINT upozorneni_skupiny_ups_id_rodic_fkey FOREIGN KEY (ups_id_rodic) REFERENCES announcement(id) ON UPDATE CASCADE ON DELETE CASCADE;

ALTER TABLE ONLY announcement_audience
    DROP CONSTRAINT if exists announcement_audience_announcement_id_fkey,
    ADD CONSTRAINT announcement_audience_announcement_id_fkey FOREIGN KEY (announcement_id) REFERENCES announcement(id) ON UPDATE CASCADE ON DELETE CASCADE;
