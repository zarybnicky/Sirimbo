--! Previous: sha1:d572ea3b5391f2334a731fd5f8e266a656f8f810
--! Hash: sha1:9d8f86ba83d5eaf7340a2b7a8c453b622fd8ca6c

--! split: 1-current.sql
drop function if exists payment_debtor_price;
drop function if exists payment_debtor_is_tentative;
drop function if exists skupiny_in_current_tenant;
drop function if exists couple_attendances;

alter table person drop column if exists middle_name;
alter table account drop column if exists name;
alter table upozorneni drop column if exists up_barvy;

drop function if exists payment_debtor_price;
CREATE FUNCTION payment_debtor_price(p payment_debtor) RETURNS price
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT (ROW(((sum(payment_recipient.amount) / (( SELECT count(*)
            FROM payment_debtor
           WHERE (p.payment_id = payment_debtor.payment_id)))::numeric))::numeric(19,4), (min(account.currency))::text))::price AS "row"
    FROM payment_recipient JOIN account ON payment_recipient.account_id = account.id
   WHERE payment_recipient.payment_id = p.payment_id;
END;
COMMENT ON FUNCTION payment_debtor_price(p payment_debtor) IS '@simpleCollections only';
GRANT ALL ON FUNCTION payment_debtor_price(p payment_debtor) TO anonymous;

comment on column couple.legacy_pary_id is '@omit';
comment on column person.legacy_user_id is '@omit';

COMMENT ON TABLE public.posting IS '@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.account IS '@omit create,update,delete
@simpleCollections only';
COMMENT ON TABLE public.payment IS '@omit create,delete
@simpleCollections only';
COMMENT ON TABLE public.event_instance IS '@omit create,delete
@simpleCollections only';

COMMENT ON TABLE public.accounting_period IS '@omit ';

create or replace function event_instance_trainer_name(t event_instance_trainer) returns text stable language sql
begin atomic
  select concat_ws(' ', prefix_title, first_name, last_name) || (case suffix_title when '' then '' else ', ' || suffix_title end)
  from person where t.person_id=person.id;
end;
grant all on function event_instance_trainer_name to anonymous;

CREATE or replace FUNCTION public.event_trainer_name(t public.event_trainer) RETURNS text
    LANGUAGE sql STABLE
    BEGIN ATOMIC
  select concat_ws(' ', prefix_title, first_name, last_name) || (case suffix_title when '' then '' else ', ' || suffix_title end)
  FROM person WHERE t.person_id = person.id;
END;
GRANT ALL ON FUNCTION public.event_trainer_name to anonymous;

drop function if exists person_name;
