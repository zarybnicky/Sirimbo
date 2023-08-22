CREATE TABLE public.person_address (
    person_id bigint NOT NULL,
    address_id bigint NOT NULL,
    is_primary boolean DEFAULT false,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    address public.address_domain
);

COMMENT ON TABLE public.person_address IS '@omit create,update,delete
@simpleCollections only';
COMMENT ON COLUMN public.person_address.address_id IS '@omit';

GRANT ALL ON TABLE public.person_address TO anonymous;
ALTER TABLE public.person_address ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.person_address
    ADD CONSTRAINT person_address_pkey PRIMARY KEY (person_id, address_id);
ALTER TABLE ONLY public.person_address
    ADD CONSTRAINT person_address_address_id_fkey FOREIGN KEY (address_id) REFERENCES public.address(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.person_address
    ADD CONSTRAINT person_address_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.person_address TO administrator USING (true);
CREATE POLICY admin_personal ON public.person_address USING ((person_id IN ( SELECT public.my_person_ids() AS my_person_ids)));
CREATE POLICY view_visible_person ON public.person_address FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.person
  WHERE (person_address.person_id = person.id))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.person_address FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _200_primary BEFORE INSERT OR UPDATE ON public.person_address FOR EACH ROW EXECUTE FUNCTION app_private.tg__person_address_primary();

CREATE INDEX person_address_address_id_idx ON public.person_address USING btree (address_id);
CREATE INDEX person_address_person_id_idx ON public.person_address USING btree (person_id);