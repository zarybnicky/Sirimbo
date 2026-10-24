import { sqlValueWithCodec, TYPES } from 'postgraphile/@dataplan/pg';
import { sql, type SQL } from 'postgraphile/pg-sql2';
import { addPgTableCondition, extendSchema } from 'postgraphile/utils';

type ParticipantCondition = {
  personIds?: string[] | null;
  coupleId?: string | null;
  cohortId?: string | null;
};

type DateCondition = {
  since?: string | null;
  until?: string | null;
};

function participantPredicate(
  column: SQL,
  scope?: ParticipantCondition | null,
): SQL | null {
  const { personIds, coupleId, cohortId } = scope ?? {};
  if (personIds?.length === 0) return sql`false`;

  const people = [];
  if (personIds != null)
    people.push(
      sql`link.person_id in (${sql.join(
        personIds.map((id) => sqlValueWithCodec(id, TYPES.bigint)),
        ', ',
      )})`,
    );
  if (personIds == null && coupleId == null)
    people.push(
      sql`link.person_id in (
        select person_id from public.current_tenant_membership
        union all
        select person_id from public.current_tenant_trainer
        union all
        select person_id from public.current_tenant_administrator
      )`,
    );
  if (cohortId != null)
    people.push(sql`link.person_id in (
      select person_id from public.current_cohort_membership
      where cohort_id = ${sqlValueWithCodec(cohortId, TYPES.bigint)}
    )`);

  // Evaluate each scope once; array predicates push into both UNION branches.
  const predicates = [];
  if (people.length)
    predicates.push(sql`${column} = any(array(
      select cc.competitor_id
      from public.person_federation_link link
      join federated.person fp
        on (fp.federation, fp.external_id) = (link.federation, link.external_id)
      join federated.competitor_component cc on cc.person_id = fp.id
      where ${sql.join(people, ' and ')}
    ))`);
  if (coupleId != null)
    predicates.push(sql`${column} = any(array(
      select competitor_id from public.couple_federation_link
      where couple_id = ${sqlValueWithCodec(coupleId, TYPES.bigint)}
    ))`);
  return predicates.length ? sql`(${sql.join(predicates, ' and ')})` : null;
}

function datePredicate(column: SQL, date?: DateCondition | null): SQL | null {
  const predicates = [];
  if (date?.since != null)
    predicates.push(sql`${column} >= ${sqlValueWithCodec(date.since, TYPES.date)}`);
  if (date?.until != null)
    predicates.push(sql`${column} < ${sqlValueWithCodec(date.until, TYPES.date)}`);
  return predicates.length ? sql`(${sql.join(predicates, ' and ')})` : null;
}

const plugins: GraphileConfig.Plugin[] = [
  extendSchema({
    typeDefs: `
      """Intersect local person, couple, and cohort selectors. Without person or couple IDs, use current tenant members, trainers, and administrators."""
      input CompetitionParticipantCondition {
        """Local person IDs. An empty list matches nobody."""
        personIds: [BigInt!]
        """An exact local partnership, across federations."""
        coupleId: BigInt
        """Require a current member of this local cohort."""
        cohortId: BigInt
      }
      input CompetitionParticipationFilter {
        participant: CompetitionParticipantCondition
        date: CompetitionDateCondition
      }
      input CompetitionDateCondition {
        """Inclusive event or competition start date."""
        since: Date
        """Exclusive event or competition start date."""
        until: Date
      }
    `,
  }),
  addPgTableCondition(
    { schemaName: 'federated', tableName: 'competition' },
    'participation',
    (build) => ({ type: build.getInputTypeByName('CompetitionParticipationFilter') }),
    (value, { sqlTableAlias: row }) => {
      if (value == null) return null;
      const { participant, date } = value as {
        participant?: ParticipantCondition | null;
        date?: DateCondition | null;
      };
      const dates = datePredicate(sql`c.start_date`, date);
      const predicates = [
        participantPredicate(sql`p.competitor_id`, participant),
        // Resolve the window before scanning participation, using its composite indexes.
        dates
          ? sql`p.competition_id = any(array(
          select c.id from federated.competition c where ${dates}
        ))`
          : null,
      ].filter((predicate) => predicate !== null);
      return sql`${row}.id = any(array(
        select p.competition_id from federated.competition_participation p
        ${predicates.length ? sql`where ${sql.join(predicates, ' and ')}` : sql``}
      ))`;
    },
  ),
  ...(['competition_entry', 'competition_result'] as const).map((tableName) =>
    addPgTableCondition(
      { schemaName: 'federated', tableName },
      'participant',
      (build) => ({ type: build.getInputTypeByName('CompetitionParticipantCondition') }),
      (value, { sqlTableAlias: row }) =>
        participantPredicate(
          sql`${row}.competitor_id`,
          value as ParticipantCondition | null,
        ),
    ),
  ),
  ...(['event', 'competition'] as const).map((tableName) =>
    addPgTableCondition(
      { schemaName: 'federated', tableName },
      'date',
      (build) => ({ type: build.getInputTypeByName('CompetitionDateCondition') }),
      (value, { sqlTableAlias: row }) =>
        datePredicate(sql`${row}.start_date`, value as DateCondition | null),
    ),
  ),
];

export default plugins;
