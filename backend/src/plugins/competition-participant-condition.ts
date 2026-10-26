import { sqlValueWithCodec, TYPES, type PgCondition } from 'postgraphile/@dataplan/pg';
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

declare global {
  namespace DataplanPg {
    interface PgConditionExtensions {
      competitionDate?: DateCondition | null;
      competitionParticipant?: boolean;
    }
  }
}

function participantPredicate(
  column: SQL,
  scope?: ParticipantCondition | null,
): SQL | null {
  if (scope == null) return null;
  const { personIds, coupleId, cohortId } = scope;
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
    'participant',
    (build) => ({
      type: build.getInputTypeByName('CompetitionParticipantCondition'),
      apply(condition: PgCondition, value: ParticipantCondition | null) {
        if (value == null) return;
        const row = condition.alias;
        condition.extensions.competitionParticipant = true;
        // Compile after sibling conditions have been applied, so the flat date
        // filter can still restrict participation before scanning its history.
        condition.where(() => {
          const predicates = [
            participantPredicate(sql`p.competitor_id`, value as ParticipantCondition),
          ];
          const dates = datePredicate(sql`c.start_date`, condition.extensions.competitionDate);
          if (dates) predicates.push(sql`p.competition_id = any(array(
            select c.id from federated.competition c where ${dates}
          ))`);
          const filters = predicates.filter((predicate) => predicate !== null);
          return sql`${row}.id = any(array(
            select p.competition_id from federated.competition_participation p
            ${filters.length ? sql`where ${sql.join(filters, ' and ')}` : sql``}
          ))`;
        });
      },
    }),
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
      (build) => ({
        type: build.getInputTypeByName('CompetitionDateCondition'),
        apply(condition: PgCondition, value: DateCondition | null) {
          const predicate = datePredicate(sql`${condition.alias}.start_date`, value);
          if (tableName === 'competition') {
            condition.extensions.competitionDate = value;
            if (predicate) condition.where(() => condition.extensions.competitionParticipant
              ? sql`true` : predicate);
          } else if (predicate) condition.where(predicate);
        },
      }),
    ),
  ),
];

export default plugins;
