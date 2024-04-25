CREATE FUNCTION public.account_assets(a public.account, since timestamp with time zone, until timestamp with time zone) RETURNS numeric
    LANGUAGE sql STABLE
    BEGIN ATOMIC
 SELECT COALESCE(sum(s.s), 0.0) AS "coalesce"
    FROM ( SELECT 0 AS s
         UNION
          SELECT COALESCE(sum(posting.amount), 0.0) AS s
            FROM (public.posting
              JOIN public.transaction ON ((posting.transaction_id = transaction.id)))
           WHERE (((account_assets.a).id = posting.account_id) AND (posting.amount > 0.0) AND (transaction.effective_date >= account_assets.since) AND (transaction.effective_date <= account_assets.until))
           GROUP BY posting.account_id) s;
END;

GRANT ALL ON FUNCTION public.account_assets(a public.account, since timestamp with time zone, until timestamp with time zone) TO anonymous;


