CREATE TABLE public.address (
    id bigint NOT NULL,
    street text NOT NULL,
    conscription_number text DEFAULT ''::text NOT NULL,
    orientation_number text DEFAULT ''::text NOT NULL,
    district text DEFAULT ''::text NOT NULL,
    city text NOT NULL,
    postal_code text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.address IS '@omit';

GRANT ALL ON TABLE public.address TO anonymous;
ALTER TABLE public.address ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.address
    ADD CONSTRAINT address_pkey PRIMARY KEY (id);

CREATE POLICY admin_all ON public.address TO administrator USING (true);
CREATE POLICY admin_personal ON public.address USING ((id IN ( SELECT person_address.address_id
   FROM public.person_address
  WHERE (person_address.person_id IN ( SELECT public.my_person_ids() AS my_person_ids)))));
CREATE POLICY view_visible_person ON public.address FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.person_address
  WHERE (person_address.address_id = address.id))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.address FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
