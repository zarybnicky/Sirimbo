import { listOfCodec, sqlValueWithCodec, TYPES } from 'postgraphile/@dataplan/pg';
import { sql } from 'postgraphile/pg-sql2';
import { addPgTableCondition, extendSchema } from 'postgraphile/utils';

type SuggestionCondition = {
  participant?: {
    personIds?: string[] | null;
    coupleId?: string | null;
    cohortId?: string | null;
  } | null;
  date: { since: string; until?: string | null };
  registeredOnly?: boolean | null;
};

const plugins: GraphileConfig.Plugin[] = [
  extendSchema({
    typeDefs: `
      input CompetitionSuggestionDateCondition {
        """Inclusive competition start date."""
        since: Date!
        """Exclusive competition start date."""
        until: Date
      }
      input CompetitionSuggestionCondition {
        """Local people, active couple, or cohort. Defaults to current tenant members, trainers, and administrators."""
        participant: CompetitionParticipantCondition
        date: CompetitionSuggestionDateCondition!
        """Only matches with an existing, non-cancelled participation."""
        registeredOnly: Boolean
      }
    `,
  }),
  ...(['event', 'competition'] as const).map((tableName) =>
    addPgTableCondition(
      { schemaName: 'federated', tableName },
      'suggestedFor',
      (build) => ({ type: build.getInputTypeByName('CompetitionSuggestionCondition') }),
      (value, { sqlTableAlias: row }) => {
        if (value == null) return null;
        const { participant: scope, date, registeredOnly } = value as SuggestionCondition;
        const participant = scope ?? {};
        return sql`${row}.id in (
        select ${tableName === 'event' ? sql`c.event_id` : sql`m.competition_id`}
        from public.suggested_competition_matches(
          ${sqlValueWithCodec(date.since, TYPES.date)},
          ${sqlValueWithCodec(date.until ?? null, TYPES.date)},
          ${sqlValueWithCodec(participant.personIds ?? null, listOfCodec(TYPES.bigint))},
          ${sqlValueWithCodec(participant.coupleId ?? null, TYPES.bigint)},
          ${sqlValueWithCodec(participant.cohortId ?? null, TYPES.bigint)},
          null,
          ${registeredOnly ? sql`true` : sql`false`}
        ) m
        ${tableName === 'event' ? sql`join federated.competition c on c.id = m.competition_id` : sql``}
      )`;
      },
    ),
  ),
];

export default plugins;
