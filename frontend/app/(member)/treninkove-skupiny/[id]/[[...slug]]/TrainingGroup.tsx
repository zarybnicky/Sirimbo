'use client';


import { DotSeparated } from '@/ui/DotSeparated';

import {
  CohortWithMembersDocument,
  UpdateCohortDocument,
  type CohortWithMembersQuery,
} from '@/graphql/Cohorts';
import { useMutation, useQuery } from 'urql';
import { cohortActions } from '@/lib/actions/cohort';
import { cohortMembershipActions } from '@/lib/actions/cohortMembership';
import { useActionMap, useActions } from '@/lib/actions';
import { RichTextView } from '@/ui/RichTextView';
import { formatAgeGroup, formatClass, getBestCstsProgress } from '@/ui/csts';
import { PageHeader } from '@/ui/PageHeader';
import { ImageEditorDialog } from '@/ui/ImageEditorDialog';
import { PersonActivityBadges } from '@/ui/PersonActivityBadges';
import { PeopleAccessOverview } from '@/ui/PeopleAccessOverview';
import Link from 'next/link';
import React from 'react';
import { ActionRow } from '@/ui/ActionRow';
import { personActions } from '@/lib/actions/person';
import { isTruthy } from '@/lib/truthyFilter';
import { CompetitionHistory, CompetitionSuggestions } from '@/ui/Competitions';
import { ActivityTimeline } from '@/ui/ActivityTimeline';
import { useAuth } from '@/lib/auth';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { parseAsString, useQueryState } from 'nuqs';
import { cn } from '@/lib/cn';
import { buttonCls, buttonGroupCls } from '@/ui/style';
import { formatCount, formatPointsAndFinals } from '@/ui/format';

export function TrainingGroup({
  initialCohort,
}: {
  initialCohort: NonNullable<CohortWithMembersQuery['entity']>;
}) {
  const auth = useAuth();
  const [editingImage, setEditingImage] = React.useState(false);
  const [, updateImage] = useMutation(UpdateCohortDocument);
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
      {editingImage && (
        <ImageEditorDialog
          file={cohort.image}
          onClose={() => setEditingImage(false)}
          onSave={async (imageId) => {
            const result = await updateImage({ id: cohort.id, patch: { imageId } });
            if (result.error) throw result.error;
          }}
        />
      )}
      <PageHeader
        imageUrl={cohort.image?.url}
        onEditImage={actions.some((action) => action.id === 'cohort.edit') ? () => setEditingImage(true) : undefined}
        title={cohort.name}
        breadcrumbLabel="Skupina"
        breadcrumbs={[{ label: 'Tréninkové skupiny', href: '/treninkove-skupiny' }]}
        subtitle={
          <DotSeparated>
            {cohort.location}
            {formatCount(members.length, { one: 'člen', few: 'členové', other: 'členů' })}
          </DotSeparated>
        }
        actions={actions}
      />


      <RichTextView value={description} />

      {auth.isLoggedIn && (
        <TabMenu selected={tab} onSelect={setTab}>
          <Tab id="members" title={`Členové (${members.length})`}>
            <TrainingGroupMembers members={members} showActivity={auth.isTrainer} />
          </Tab>
          <Tab id="fed" title="Výsledky">
            <CompetitionHistory participant={{ cohortId: cohort.id }} />
          </Tab>
          <Tab id="eligible" title="Možné soutěže">
            <CompetitionSuggestions participant={{ cohortId: cohort.id }} />
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
    <div className="flex gap-0.5 items-baseline justify-between leading-tight text-center">
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
      <div className="whitespace-nowrap text-sm text-neutral-11">
        {formatPointsAndFinals(
          item.points ?? '0',
          item.domesticFinals + item.foreignFinals,
        )}
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
  const [view, setView] = React.useState<'activity' | 'competitive'>(
    showActivity ? 'activity' : 'competitive',
  );
  const activityView = showActivity && view === 'activity';
  const membershipActionMap = useActionMap(cohortMembershipActions, members);
  const memberActionMap = useActionMap(
    personActions,
    members.map((x) => x.person).filter(isTruthy),
  );

  return (
    <div
      className={cn(
        'grid gap-x-4 gap-y-3 pb-4',
        activityView
          ? 'grid-cols-1 lg:grid-cols-[minmax(0,1fr)_max-content]'
          : 'grid-cols-[minmax(0,1fr)_max-content_max-content] lg:grid-cols-[minmax(0,1fr)_max-content_max-content_max-content]',
      )}
    >
      {showActivity && (
        <div
          role="group"
          aria-label="Zobrazení členů"
          className={buttonGroupCls({ className: 'col-span-full justify-self-end' })}
        >
          {(
            [
              ['activity', 'Aktivita'],
              ['competitive', 'Soutěžní info'],
            ] as const
          ).map(([mode, label]) => (
            <button
              key={mode}
              type="button"
              aria-pressed={view === mode}
              className={buttonCls({
                variant: view === mode ? 'primary' : 'outline',
                size: 'sm',
              })}
              onClick={() => setView(mode)}
            >
              {label}
            </button>
          ))}
        </div>
      )}
      {members.map((membership) => (
        <div
          key={membership.id}
          className="col-span-full grid grid-cols-subgrid items-center gap-x-5 gap-y-3 text-sm"
        >
          <ActionRow
            className={cn(
              'mb-0 min-w-0',
              activityView ? 'col-span-1' : 'col-span-full lg:col-span-1',
            )}
            actions={[
              ...(membership.person ? memberActionMap.get(membership.person.id)! : []),
              ...membershipActionMap.get(membership.id)!,
            ]}
          >
            {membership.person ? (
              <Link
                className="min-w-0 font-bold underline"
                href={`/clenove/${membership.person.id}`}
              >
                {membership.person.name}
              </Link>
            ) : (
              '?'
            )}
          </ActionRow>

          {activityView && membership.person?.lastActivity && (
            <div className="min-w-0 lg:justify-self-end">
              <PersonActivityBadges activity={membership.person.lastActivity} />
            </div>
          )}
          {!activityView && (
            <>
              <div className="font-medium text-right">
                {formatAgeGroup(membership.person?.ageGroup)}
              </div>
              <div>
                <CategoryList person={membership.person} discipline="Standard" />
              </div>
              <div>
                <CategoryList person={membership.person} discipline="Latin" />
              </div>
            </>
          )}
        </div>
      ))}
    </div>
  );
});
