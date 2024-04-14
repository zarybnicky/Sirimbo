CREATE TABLE public.posting (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    transaction_id bigint NOT NULL,
    account_id bigint NOT NULL,
    original_account_id bigint,
    amount numeric(19,4),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.posting IS '@omit create,update,delete
@simpleCollections both';

GRANT ALL ON TABLE public.posting TO anonymous;
ALTER TABLE public.posting ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.posting
    ADD CONSTRAINT posting_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.posting
    ADD CONSTRAINT posting_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.account(id);
ALTER TABLE ONLY public.posting
    ADD CONSTRAINT posting_original_account_id_fkey FOREIGN KEY (original_account_id) REFERENCES public.account(id);
ALTER TABLE ONLY public.posting
    ADD CONSTRAINT posting_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.posting
    ADD CONSTRAINT posting_transaction_id_fkey FOREIGN KEY (transaction_id) REFERENCES public.transaction(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_manage ON public.posting TO administrator USING (true);
CREATE POLICY my_tenant ON public.posting AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));
CREATE POLICY person_view ON public.posting FOR SELECT TO anonymous USING (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.posting FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _900_fix_balance_entries AFTER INSERT OR DELETE OR UPDATE OF amount OR TRUNCATE ON public.posting FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_account_balances__update();
