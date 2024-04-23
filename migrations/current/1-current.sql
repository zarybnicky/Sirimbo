create or replace function account_liabilities(a account, since timestamptz, until timestamptz) returns numeric(19,4) stable
begin atomic
  SELECT COALESCE(SUM(s), 0.0) FROM (
    SELECT 0 as s
    UNION
    SELECT COALESCE(sum(amount), 0.0) as s
     FROM posting
     JOIN transaction on transaction_id = transaction.id
    WHERE a.id = account_id AND amount < 0.0 AND effective_date >= since AND effective_date <= until
    GROUP BY account_id
  ) s;
end;

create or replace function account_assets(a account, since timestamptz, until timestamptz) returns numeric(19,4) stable
begin atomic
  SELECT COALESCE(SUM(s), 0.0) FROM (
    SELECT 0 as s
    UNION
    SELECT COALESCE(sum(amount), 0.0) as s
     FROM posting
     JOIN transaction on transaction_id = transaction.id
    WHERE a.id = account_id AND amount > 0.0 AND effective_date >= since AND effective_date <= until
    GROUP BY account_id
  ) s;
end;

grant all on function account_liabilities to anonymous;
grant all on function account_assets to anonymous;
