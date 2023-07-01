CREATE FUNCTION public.on_delete_file_dokumenty() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    perform graphile_worker.add_job('delete_file', json_build_object('path', OLD.d_path));
    return old;
END;
$$;



