import { LIST_TYPES, TYPES } from 'postgraphile/@dataplan/pg';
import { addPgTableCondition, extendSchema } from 'postgraphile/utils';

type EventRangeCondition = { start: string; end?: string | null };
type EventScopeCondition = {
  mode?: 'all' | 'top_level' | 'mine' | 'relevant' | null;
  parentId?: string | null;
};

const plugins: GraphileConfig.Plugin[] = [
  extendSchema({
    typeDefs: `
      input EventRangeCondition {
        """Inclusive start of the time window."""
        start: Datetime!
        """Exclusive end of the time window. Omit for an unbounded end."""
        end: Datetime
      }
      input EventScopeCondition {
        mode: EventInstanceRangeScope = ALL
        parentId: BigInt
      }
    `,
  }),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'event' },
    'overlaps',
    (build) => ({ type: build.getInputTypeByName('EventRangeCondition') }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => {
      if (value == null) return null;
      const { start, end } = value as EventRangeCondition;
      const since = sqlValueWithCodec(start, TYPES.timestamptz);
      const until = sqlValueWithCodec(end ?? null, TYPES.timestamptz);
      return sql`app_private.tstzrange_overlaps(${row}.range, tstzrange(${since}, ${until}, '[)'))`;
    },
  ),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'event' },
    'scope',
    (build) => ({ type: build.getInputTypeByName('EventScopeCondition') }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => {
      if (value == null) return null;
      const { mode = 'all', parentId } = value as EventScopeCondition;
      const mine = sql`
        select event_id from public.event_registration
        where person_id = any((select public.current_person_ids())::bigint[])
          and registration_status = 'active'
        union all
        select event_id from public.event_trainer
        where person_id = any((select public.current_person_ids())::bigint[])
      `;
      if (parentId != null) {
        const parent = sql`${row}.parent_id = ${sqlValueWithCodec(parentId, TYPES.bigint)}`;
        return mode === 'mine' || mode === null
          ? sql`${parent} and ${row}.id in (${mine})`
          : parent;
      }
      switch (mode) {
        case 'all': return null;
        case 'top_level': return sql`${row}.parent_id is null`;
        case 'mine': return sql`${row}.id in (${mine})`;
        case 'relevant': return sql`(${row}.parent_id is null
          or ${row}.id in (${mine}) or ${row}.parent_id in (${mine}))`;
        default: return sql`false`;
      }
    },
  ),
  ...(['types', 'trainerIds', 'participantIds', 'locationIds'] as const).map((field) =>
    addPgTableCondition(
      { schemaName: 'public', tableName: 'event' },
      field,
      (build) => {
        const type = build.getInputTypeByName(field === 'types' ? 'EventType' : 'BigInt');
        return {
          type: new build.graphql.GraphQLList(
            field === 'types' ? new build.graphql.GraphQLNonNull(type) : type,
          ),
        };
      },
      (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) => {
        const values = value as (string | null)[] | null;
        if (values == null) return null;
        if (values.length === 0) return sql`false`;
        if (field === 'types')
          return sql`${row}.type::text = any(${sqlValueWithCodec(values, LIST_TYPES.text)})`;
        const ids = sqlValueWithCodec(values, LIST_TYPES.bigint);
        if (field === 'locationIds') return sql`${row}.location_id = any(${ids})`;
        return sql`exists (
          select 1 from ${sql.identifier('public', field === 'trainerIds' ? 'event_trainer' : 'event_registration')} p
          where p.event_id = ${row}.id and p.person_id = any(${ids})
            ${field === 'participantIds' ? sql`and p.registration_status = 'active'` : sql.blank}
        )`;
      },
    ),
  ),
];

export default plugins;
