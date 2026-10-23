create or replace function app_private.tstzrange_overlaps_internal(tstzrange, anyrange)
  returns boolean
  language internal immutable strict parallel safe leakproof
as 'range_overlaps';

do $$
begin
  if not exists (
    select from pg_catalog.pg_operator
    where oprnamespace = 'app_private'::regnamespace
      and oprname = '&&'
      and oprleft = 'tstzrange'::regtype
      and oprright = 'anyrange'::regtype
  ) then
    create operator app_private.&& (
      leftarg = tstzrange,
      rightarg = anyrange,
      function = app_private.tstzrange_overlaps_internal
    );
  end if;

  -- rangesel recognizes built-in operator OIDs only.
  alter operator app_private.&& (tstzrange, anyrange) set (
    restrict = pg_catalog.matchingsel,
    join = pg_catalog.matchingjoinsel
  );

  -- GiST's range support dispatches on the right operand's declared type.
  -- Keep it polymorphic so strategy 3 uses its range-overlap implementation.
  if not exists (
    select from pg_catalog.pg_amop member
    join pg_catalog.pg_opfamily family on family.oid = member.amopfamily
    join pg_catalog.pg_am method on method.oid = family.opfmethod
    where family.opfnamespace = 'pg_catalog'::regnamespace
      and family.opfname = 'range_ops'
      and method.amname = 'gist'
      and member.amopopr = 'app_private.&&(tstzrange,anyrange)'::regoperator
      and member.amopstrategy = 3
  ) then
    alter operator family pg_catalog.range_ops using gist
      add operator 3 app_private.&& (tstzrange, anyrange);
  end if;
end;
$$;

-- This wrapper inlines to the registered operator, so existing GiST indexes
-- can use the overlap condition before RLS. Put the indexed range first.
create or replace function app_private.tstzrange_overlaps(tstzrange, tstzrange)
  returns boolean
  language sql immutable strict parallel safe leakproof
return $1 operator(app_private.&&) $2;

grant execute on function app_private.tstzrange_overlaps_internal, app_private.tstzrange_overlaps to anonymous;
