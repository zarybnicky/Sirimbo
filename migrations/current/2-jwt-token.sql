DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_attribute a JOIN pg_type t ON t.typrelid = a.attrelid
    WHERE t.oid = to_regtype('public.jwt_token') AND a.attname = 'guest_tenant_ids'
  ) THEN
    ALTER TYPE public.jwt_token ADD ATTRIBUTE guest_tenant_ids bigint[];
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM pg_attribute a JOIN pg_type t ON t.typrelid = a.attrelid
    WHERE t.oid = to_regtype('public.jwt_token') AND a.attname = 'member_tenant_ids'
  ) THEN
    ALTER TYPE public.jwt_token ADD ATTRIBUTE member_tenant_ids bigint[];
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM pg_attribute a JOIN pg_type t ON t.typrelid = a.attrelid
    WHERE t.oid = to_regtype('public.jwt_token') AND a.attname = 'trainer_tenant_ids'
  ) THEN
    ALTER TYPE public.jwt_token ADD ATTRIBUTE trainer_tenant_ids bigint[];
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM pg_attribute a JOIN pg_type t ON t.typrelid = a.attrelid
    WHERE t.oid = to_regtype('public.jwt_token') AND a.attname = 'admin_tenant_ids'
  ) THEN
    ALTER TYPE public.jwt_token ADD ATTRIBUTE admin_tenant_ids bigint[];
  END IF;
END
$$ LANGUAGE plpgsql;

--!include functions/create_jwt_token.sql
