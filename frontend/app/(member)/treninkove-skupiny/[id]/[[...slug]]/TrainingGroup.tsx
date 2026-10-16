'use client';

import {
  CohortWithMembersDocument,
  type CohortWithMembersQuery,
} from '@/graphql/Cohorts';
import { useQuery } from 'urql';
import { cohortActions } from '@/lib/actions/cohort';
import { cohortMembershipActions } from '@/lib/actions/cohortMembership';
import { useActionMap, useActions } from '@/lib/actions';
import { RichTextView } from '@/ui/RichTextView';
import { formatAgeGroup, formatClass, getBestCstsProgress } from '@/ui/csts';
import { PageHeader } from '@/ui/PageHeader';
import { PersonActivityBadges } from '@/ui/PersonActivityBadges';
import { PeopleAccessOverview } from '@/ui/PeopleAccessOverview';
import Link from 'next/link';
import React from 'react';
import { ActionRow } from '@/ui/ActionRow';
import { personActions } from '@/lib/actions/person';
import { isTruthy } from '@/lib/truthyFilter';
import { ActivityTimeline } from '@/ui/ActivityTimeline';
import { useAuth } from '@/lib/auth';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { parseAsString, useQueryState } from 'nuqs';

export function TrainingGroup({
  initialCohort,
}: {
  initialCohort: NonNullable<CohortWithMembersQuery['entity']>;
}) {
  const auth = useAuth();
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );
  const [{ data }] = useQuery({
    query: CohortWithMembersDocument,
    variables: { id: initialCohort.id, includeActivity: auth.isTrainer },
  });
  const cohort = data?.entity ?? initialCohort;

  const members = React.useMemo(
    () => cohort.cohortMembershipsList ?? [],
    [cohort.cohortMembershipsList],
  );
  const description = React.useMemo(
    () => cohort.description?.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', ''),
    [cohort.description],
  );
  const actions = useActions(cohortActions, cohort);

  return (
    <>
      <PageHeader
        title={cohort.name}
        breadcrumbLabel="Skupina"
        breadcrumbs={[{ label: 'Tréninkové skupiny', href: '/treninkove-skupiny' }]}
        subtitle={[
          cohort.location,
          members.length +
            ' ' +
            (members.length > 4 || members.length === 0
              ? 'členů'
              : members.length > 1
                ? 'členové'
                : 'člen'),
        ]
          .filter(Boolean)
          .join(' · ')}
        actions={actions}
      />

      <RichTextView value={description} />

      {auth.isLoggedIn && (
        <TabMenu selected={tab} onSelect={setTab}>
          <Tab id="members" title={`Členové (${members.length})`}>
            <TrainingGroupMembers members={members} showActivity={auth.isTrainer} />
          </Tab>
          <Tab id="access" title="Přístupy" requireAdmin>
            <PeopleAccessOverview cohortId={cohort.id} />
          </Tab>
          <Tab id="activity" title="Aktivita">
            <ActivityTimeline cohortId={cohort.id} />
          </Tab>
        </TabMenu>
      )}
    </>
  );
}

function CategoryList({
  person,
  discipline,
}: {
  person: NonNullable<
    CohortWithMembersQuery['entity']
  >['cohortMembershipsList'][number]['person'];
  discipline: string;
}) {
  if (!person?.cstsId)
    return (
      <div className="flex justify-center">
        <div className="px-1.5 border-2 border-neutral-8 rounded-full text-neutral-9 text-xs">
          nemá IDT
        </div>
      </div>
    );
  const item = getBestCstsProgress(person.federationCategoryProgresses.nodes, discipline);
  if (!item) return null;

  return (
    <div className="flex flex-col leading-tight text-center">
      <div className="font-medium">
        {[
          formatClass(item.category?.class),
          item.category?.discipline === 'Standard_Latin'
            ? 'STT+LAT'
            : item.category?.discipline === 'Standard'
              ? 'STT'
              : item.category?.discipline === 'Latin'
                ? 'LAT'
                : item.category?.discipline,
        ]
          .filter(Boolean)
          .join(' ')}
      </div>
      <div className="text-xs text-neutral-11">
        {Number.parseFloat(item.points ?? '0')} /{' '}
        {item.domesticFinals + item.foreignFinals}F
      </div>
    </div>
  );
}

const TrainingGroupMembers = React.memo(function TrainingGroupMembers({
  members,
  showActivity,
}: {
  showActivity: boolean;
  members: NonNullable<CohortWithMembersQuery['entity']>['cohortMembershipsList'];
}) {
  const membershipActionMap = useActionMap(cohortMembershipActions, members);
  const memberActionMap = useActionMap(
    personActions,
    members.map((x) => x.person).filter(isTruthy),
  );

  return (
    <div className="grid grid-cols-[minmax(0,1fr)_max-content_max-content] gap-x-4 gap-y-3 lg:grid-cols-[minmax(0,1fr)_max-content_max-content_max-content] pb-4">
      {members.map((membership) => (
        <div
          key={membership.id}
          className="col-span-full grid grid-cols-subgrid items-center gap-x-4 gap-y-2 text-sm"
        >
          <ActionRow
            className="col-span-full mb-0 min-w-0 lg:col-span-1"
            actions={[
              ...(membership.person ? memberActionMap.get(membership.person.id)! : []),
              ...membershipActionMap.get(membership.id)!,
            ]}
          >
            {membership.person ? (
              <div className="min-w-0 flex flex-col items-start gap-1">
                <Link
                  className="font-bold underline"
                  href={`/clenove/${membership.person.id}`}
                >
                  {membership.person.name}
                </Link>
                {showActivity && membership.person.lastActivity && (
                  <PersonActivityBadges activity={membership.person.lastActivity} />
                )}
              </div>
            ) : (
              '?'
            )}
          </ActionRow>

          <div className="font-medium text-right">
            {formatAgeGroup(membership.person?.ageGroup)}
          </div>
          <div>
            <CategoryList person={membership.person} discipline="Standard" />
          </div>
          <div>
            <CategoryList person={membership.person} discipline="Latin" />
          </div>
        </div>
      ))}
    </div>
  );
});
