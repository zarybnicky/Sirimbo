CREATE TABLE crawler.refetch_policy (
    federation text NOT NULL,
    kind text NOT NULL,
    reference_offset interval,
    fetch_interval interval NOT NULL,
    CONSTRAINT refetch_policy_fetch_interval_check CHECK ((fetch_interval > '00:00:00'::interval))
);

COMMENT ON TABLE crawler.refetch_policy IS '@behavior -*';

GRANT SELECT ON TABLE crawler.refetch_policy TO system_admin;
ALTER TABLE crawler.refetch_policy ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY crawler.refetch_policy
    ADD CONSTRAINT refetch_policy_federation_kind_reference_offset_key UNIQUE NULLS NOT DISTINCT (federation, kind, reference_offset);

CREATE POLICY system_admin_read ON crawler.refetch_policy FOR SELECT TO system_admin USING (true);
