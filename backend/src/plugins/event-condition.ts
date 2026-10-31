import { LIST_TYPES, TYPES } from 'postgraphile/@dataplan/pg';
import { addPgTableCondition, extendSchema } from 'postgraphile/utils';

type EventRangeCondition = { start: string; end?: string | null };

const plugins: GraphileConfig.Plugin[] = [
  extendSchema({
    typeDefs: `
      input EventRangeCondition {
        """Inclusive start of the time window."""
        start: Datetime!
        """Exclusive end of the time window. Omit for an unbounded end."""
        end: Datetime
      }
    `,
  }),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'event' },
    'canRegister',
    (build) => ({
      type: build.graphql.GraphQLBoolean,
      description: 'Whether a self-service registration is available.',
    }),
    (value, { sql, sqlTableAlias: row, sqlValueWithCodec }) =>
      value == null
        ? null
        : sql`(public.event_registration_info(${row})).can_register = ${sqlValueWithCodec(value, TYPES.boolean)}`,
  ),
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
  ...(['onlyMine', 'relevantOnly'] as const).map((field) =>
    addPgTableCondition(
      { schemaName: 'public', tableName: 'event' },
      field,
      (build) => ({
        type: build.graphql.GraphQLBoolean,
        description: field === 'onlyMine'
          ? 'Only events where the viewer has an active registration or trainer assignment.'
          : 'Only top-level events, the viewer’s events, and children of the viewer’s events.',
      }),
      (value, { sql, sqlTableAlias: row }) => {
        if (!value) return null;
        const mine = sql`
          select event_id from public.event_registration
          where person_id = any((select public.current_person_ids())::bigint[])
            and registration_status = 'active'
          union all
          select event_id from public.event_trainer
          where person_id = any((select public.current_person_ids())::bigint[])
        `;
        return field === 'onlyMine'
          ? sql`${row}.id in (${mine})`
          : sql`(${row}.parent_id is null
              or ${row}.id in (${mine}) or ${row}.parent_id in (${mine}))`;
      },
    ),
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
          return sql`${row}.type = any(${sqlValueWithCodec(values, LIST_TYPES.text)}::public.event_type[])`;
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
