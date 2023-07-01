CREATE TABLE public.page_revision (
    rev_number integer NOT NULL,
    rev_operation character(1) NOT NULL,
    rev_timestamp timestamp without time zone,
    id integer NOT NULL,
    url character varying NOT NULL,
    content json NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    title text DEFAULT ''::text NOT NULL,
    CONSTRAINT base_revision_rev_operation_check CHECK ((rev_operation = ANY (ARRAY['I'::bpchar, 'U'::bpchar, 'D'::bpchar])))
);

COMMENT ON TABLE public.page_revision IS '@omit create,update,delete';

GRANT ALL ON TABLE public.page_revision TO anonymous;
ALTER TABLE public.page_revision ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.page_revision
    ADD CONSTRAINT page_revision_pkey PRIMARY KEY (rev_number, id);

CREATE POLICY all_view ON public.page_revision FOR SELECT USING (true);

