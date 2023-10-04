CREATE TABLE public.account (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    person_id bigint,
    name text,
    opening_balance numeric(19,4) DEFAULT 0.0 NOT NULL,
    currency public.citext DEFAULT 'CZK'::public.citext NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.account IS '@omit create,update,delete
@simpleCollections both';

GRANT ALL ON TABLE public.account TO anonymous;
ALTER TABLE public.account ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.account
    ADD CONSTRAINT account_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.account
    ADD CONSTRAINT account_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id);
ALTER TABLE ONLY public.account
    ADD CONSTRAINT account_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_manage ON public.account TO administrator USING (true);
CREATE POLICY my_tenant ON public.account AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.account FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _900_fix_balance_accounts AFTER INSERT OR DELETE OR UPDATE OF opening_balance OR TRUNCATE ON public.account FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_account_balances__update();

CREATE UNIQUE INDEX account_tenant_id_person_id_currency_idx ON public.account USING btree (tenant_id, person_id, currency);