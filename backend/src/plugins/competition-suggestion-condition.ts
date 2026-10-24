import { listOfCodec, sqlValueWithCodec, TYPES } from 'postgraphile/@dataplan/pg';
import { sql } from 'postgraphile/pg-sql2';
import { addPgTableCondition, extendSchema } from 'postgraphile/utils';

type SuggestionCondition = {
  participant?: {
    personIds?: string[] | null;
    coupleId?: string | null;
    cohortId?: string | null;
  } | null;
  date: { since: string; until: string };
};

const plugins: GraphileConfig.Plugin[] = [
  extendSchema({
    typeDefs: `
      input CompetitionSuggestionDateCondition {
        """Inclusive competition start date."""
        since: Date!
        """Exclusive competition start date."""
        until: Date!
      }
      input CompetitionSuggestionCondition {
        """Local people, active couple, or cohort. Defaults to current tenant members, trainers, and administrators."""
        participant: CompetitionParticipantCondition
        date: CompetitionSuggestionDateCondition!
      }
    `,
  }),
  addPgTableCondition(
    { schemaName: 'federated', tableName: 'competition' },
    'suggestedFor',
    (build) => ({ type: build.getInputTypeByName('CompetitionSuggestionCondition') }),
    (value, { sqlTableAlias: row }) => {
      if (value == null) return null;
      const { participant: scope, date } = value as SuggestionCondition;
      const participant = scope ?? {};
      return sql`${row}.id in (
        select competition_id from public.suggested_competition_matches(
          ${sqlValueWithCodec(date.since, TYPES.date)},
          ${sqlValueWithCodec(date.until, TYPES.date)},
          ${sqlValueWithCodec(participant.personIds ?? null, listOfCodec(TYPES.bigint))},
          ${sqlValueWithCodec(participant.coupleId ?? null, TYPES.bigint)},
          ${sqlValueWithCodec(participant.cohortId ?? null, TYPES.bigint)}
        )
      )`;
    },
  ),
];

export default plugins;
