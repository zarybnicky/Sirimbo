import { pgClassExpression, sqlValueWithCodec, TYPES, type PgCondition, type PgSelectSingleStep } from 'postgraphile/@dataplan/pg';
import { sql, type SQL } from 'postgraphile/pg-sql2';
import { addPgTableCondition, extendSchema } from 'postgraphile/utils';

type ParticipantCondition = {
  personIds?: string[] | null;
  onlyMine?: boolean | null;
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

function personPredicate(
  column: SQL,
  { personIds, onlyMine, coupleId, cohortId }: ParticipantCondition,
): SQL | null {
  if (personIds?.length === 0) return sql`false`;

  const people = [];
  if (onlyMine)
    people.push(
      sql`${column} = any((select public.current_person_ids())::bigint[])`,
    );
  if (personIds != null)
    people.push(
      sql`${column} in (${sql.join(
        personIds.map((id) => sqlValueWithCodec(id, TYPES.bigint)),
        ', ',
      )})`,
    );
  if (personIds == null && coupleId == null && !onlyMine)
    people.push(
      sql`${column} in (
        select person_id from public.current_tenant_membership
        union all
        select person_id from public.current_tenant_trainer
        union all
        select person_id from public.current_tenant_administrator
      )`,
    );
  if (cohortId != null)
    people.push(sql`${column} in (
      select person_id from public.current_cohort_membership
      where cohort_id = ${sqlValueWithCodec(cohortId, TYPES.bigint)}
    )`);

  return people.length ? sql`(${sql.join(people, ' and ')})` : null;
}

function participantPredicate(
  column: SQL,
  scope?: ParticipantCondition | null,
): SQL | null {
  if (scope == null) return null;
  const people = personPredicate(sql`link.person_id`, scope);
  // Evaluate each scope once; array predicates push into both UNION branches.
  const predicates = [];
  if (people)
    predicates.push(sql`${column} = any(array(
      select cc.competitor_id
      from public.person_federation_link link
      join federated.person fp
        on (fp.federation, fp.external_id) = (link.federation, link.external_id)
      join federated.competitor_component cc on cc.person_id = fp.id
      where ${people}
    ))`);
  if (scope.coupleId != null)
    predicates.push(sql`${column} = any(array(
      select competitor_id from public.couple_federation_link
      where couple_id = ${sqlValueWithCodec(scope.coupleId, TYPES.bigint)}
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

function federationPeople(scope: ParticipantCondition): SQL {
  const people = personPredicate(sql`link.person_id`, scope) ?? sql.true;
  const couple = scope.coupleId == null ? sql.true : sql`link.person_id in (
    select man_id from public.couple where id = ${sqlValueWithCodec(scope.coupleId, TYPES.bigint)}
    union all
    select woman_id from public.couple where id = ${sqlValueWithCodec(scope.coupleId, TYPES.bigint)}
  )`;
  return sql`select fp.id from public.person_federation_link link
    join federated.person fp on (fp.federation, fp.external_id) = (link.federation, link.external_id)
    where ${people} and ${couple}`;
}

function registrationPredicate(row: SQL, scope: ParticipantCondition): SQL {
  const predicates = [personPredicate(sql`${row}.person_id`, scope)];
  if (scope.coupleId != null) {
    const coupleId = sqlValueWithCodec(scope.coupleId, TYPES.bigint);
    predicates.push(sql`(${row}.couple_id = ${coupleId}
      or ${row}.parent_registration_id in (
        select id from public.event_registration where couple_id = ${coupleId}
      ))`);
  }
  const filters = predicates.filter((predicate) => predicate !== null);
  return filters.length ? sql`(${sql.join(filters, ' and ')})` : sql.true;
}

const plugins: GraphileConfig.Plugin[] = [
  extendSchema({
    typeDefs: `
      extend type FederationCompetitor { isMine: Boolean! }
      extend type FederationPerson { isMine: Boolean! }
      """Intersect local person, couple, and cohort selectors. Without person or couple IDs, use current tenant members, trainers, and administrators."""
      input CompetitionParticipantCondition {
        """Local person IDs. An empty list matches nobody."""
        personIds: [BigInt!]
        """Intersect the selection with all person IDs assigned to the viewer."""
        onlyMine: Boolean
        """An exact local partnership, across federations."""
        coupleId: BigInt
        """Require a current member of this local cohort."""
        cohortId: BigInt
      }
    `,
    objects: {
      FederationCompetitor: {
        plans: {
          isMine($row: PgSelectSingleStep) {
            return pgClassExpression($row, TYPES.boolean)`${$row.get('id')} in (
              select competitor_id from federated.competitor_component
              where person_id in (${federationPeople({ onlyMine: true })})
            )`;
          },
        },
      },
      FederationPerson: {
        plans: {
          isMine($row: PgSelectSingleStep) {
            return pgClassExpression($row, TYPES.boolean)`${$row.get('id')} in (
              ${federationPeople({ onlyMine: true })}
            )`;
          },
        },
      },
    },
  }),
  addPgTableCondition(
    { schemaName: 'federated', tableName: 'event' },
    'participant',
    (build) => ({
      type: build.getInputTypeByName('CompetitionParticipantCondition'),
      apply(condition: PgCondition, value: ParticipantCondition | null) {
        if (value == null) return;
        condition.where(() => {
          const date = condition.extensions.competitionDate;
          return sql`${condition.alias}.id in (
            select p.event_id from federated.competition_participation p
            join federated.competition c on c.id = p.competition_id
            where c.status <> 'cancelled'
              and ${datePredicate(sql`p.date`, date) ?? sql.true}
              and ${participantPredicate(sql`p.competitor_id`, value) ?? sql.true}
            union
            select o.event_id from federated.official_assignment o
            where ${datePredicate(sql`o.date`, date) ?? sql.true}
              and o.person_id = any(array(${federationPeople(value)}))
          )`;
        });
      },
    }),
  ),
  addPgTableCondition(
    { schemaName: 'federated', tableName: 'competition_participation' },
    'includeCancelledEntries',
    (build) => ({
      type: build.graphql.GraphQLBoolean,
      description: 'Include cancelled entries without a ranked result.',
    }),
    (value, { sqlTableAlias: row }) => value === false
      ? sql`(not ${row}.cancelled or ${row}.ranking is not null)` : null,
  ),
  ...(['event', 'competition_participation', 'official_assignment'] as const).map((tableName) =>
    addPgTableCondition(
      { schemaName: 'federated', tableName },
      tableName === 'event' ? 'participationDate' : 'dateRange',
      (build) => ({
        type: build.getInputTypeByName('DateRangeCondition'),
        description: tableName === 'event'
          ? 'Date bounds for the participant condition. Use date for the event start date.'
          : 'Inclusive start and exclusive end of the participation or assignment dates.',
        apply(condition: PgCondition, value: DateCondition | null) {
          condition.extensions.competitionDate = value;
          if (tableName !== 'event') {
            const predicate = datePredicate(sql`${condition.alias}.date`, value);
            if (predicate) condition.where(predicate);
          }
        },
      }),
    ),
  ),
  addPgTableCondition(
    { schemaName: 'federated', tableName: 'official_assignment' },
    'person',
    (build) => ({ type: build.getInputTypeByName('CompetitionParticipantCondition') }),
    (value, { sqlTableAlias: row }) => value == null ? null
      : sql`${row}.person_id = any(array(${federationPeople(value as ParticipantCondition)}))`,
  ),
  addPgTableCondition(
    { schemaName: 'federated', tableName: 'competition_participation' },
    'includeCancelledCompetitions',
    (build) => ({ type: build.graphql.GraphQLBoolean }),
    (value, { sqlTableAlias: row }) => value === false
      ? sql`${row}.competition_id in (select id from federated.competition where status <> 'cancelled')` : null,
  ),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'event_registration' },
    'participant',
    (build) => ({ type: build.getInputTypeByName('CompetitionParticipantCondition') }),
    (value, { sqlTableAlias: row }) => value == null
      ? null : registrationPredicate(row, value as ParticipantCondition),
  ),
  addPgTableCondition(
    { schemaName: 'public', tableName: 'event' },
    'participant',
    (build) => ({ type: build.getInputTypeByName('CompetitionParticipantCondition') }),
    (value, { sqlTableAlias: row }) => {
      if (value == null) return null;
      const scope = value as ParticipantCondition;
      const { personIds, coupleId, cohortId, onlyMine } = scope;
      if (personIds == null && coupleId == null && cohortId == null && !onlyMine) return null;
      const registration = sql.identifier(Symbol('registration'));
      const registered = sql`exists (select 1 from public.event_registration ${registration}
        where ${registration}.event_id = ${row}.id
          and ${registration}.registration_status = 'active'
          and ${registrationPredicate(registration, scope)})`;
      if (coupleId != null) return registered;
      const trainer = sql.identifier(Symbol('trainer'));
      return sql`(${registered} or exists (
        select 1 from public.event_trainer ${trainer}
        where ${trainer}.event_id = ${row}.id
          and ${personPredicate(sql`${trainer}.person_id`, scope)!}
      ))`;
    },
  ),
  addPgTableCondition(
    { schemaName: 'federated', tableName: 'competition' },
    'competitor',
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
          const dates = datePredicate(
            sql`c.start_date`,
            condition.extensions.competitionDate,
          );
          if (dates)
            predicates.push(sql`p.competition_id = any(array(
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
  ...(['competition_entry', 'competition_result', 'competition_participation'] as const).map((tableName) =>
    addPgTableCondition(
      { schemaName: 'federated', tableName },
      'competitor',
      (build) => ({ type: build.getInputTypeByName('CompetitionParticipantCondition') }),
      (value, { sqlTableAlias: row }) => {
        const scope = value as ParticipantCondition | null;
        if (tableName !== 'competition_participation' || scope == null || scope.coupleId != null)
          return participantPredicate(sql`${row}.competitor_id`, scope);
        // These rows are already restricted by event/competition. A semijoin avoids
        // probing the entry index once for every member of the club.
        return sql`${row}.competitor_id in (
          select competitor_id from federated.competitor_component
          where person_id = any(array(${federationPeople(scope)}))
        )`;
      },
    ),
  ),
  ...(['event', 'competition', 'competition_result'] as const).map((tableName) =>
    addPgTableCondition({ schemaName: 'federated', tableName }, 'date', (build) => ({
      type: build.getInputTypeByName('DateRangeCondition'),
      apply(condition: PgCondition, value: DateCondition | null) {
        if (tableName === 'competition_result') {
          const predicate = datePredicate(sql`c.start_date`, value);
          if (predicate)
            condition.where(sql`${condition.alias}.competition_id = any(array(
              select c.id from federated.competition c where ${predicate}
            ))`);
          return;
        }
        const predicate = datePredicate(sql`${condition.alias}.start_date`, value);
        if (tableName === 'competition') {
          condition.extensions.competitionDate = value;
          if (predicate)
            condition.where(() =>
              condition.extensions.competitionParticipant ? sql`true` : predicate,
            );
        } else if (predicate) condition.where(predicate);
      },
    })),
  ),
];

export default plugins;
