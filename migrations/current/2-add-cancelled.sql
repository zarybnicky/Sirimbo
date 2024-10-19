do $$
begin
    if not exists (
        select 1 from pg_catalog.pg_enum as enum_value
        inner join pg_catalog.pg_type as custom_type on custom_type.oid = enum_value.enumtypid
        where typname = 'attendance_type' and enumlabel = 'cancelled'
    ) then
        alter type attendance_type add value 'cancelled';
    end if;
end$$;
