CREATE TYPE public.login_result AS (
	usr public.users,
	jwt public.jwt_token
);

COMMENT ON TYPE public.login_result IS '@name result';
