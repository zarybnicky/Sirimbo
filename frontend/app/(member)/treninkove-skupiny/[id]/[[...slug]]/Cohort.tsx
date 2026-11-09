'use client';

import { DotSeparated } from '@/ui/DotSeparated';

import {
  CohortDocument,
  type CohortFragment,
  type CohortMemberFragment,
  UpdateCohortDocument,
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
import React from 'react';
import { ActionRow } from '@/ui/ActionRow';
import { personActions } from '@/lib/actions/person';
import { CompetitionHistory, CompetitionSuggestions } from '@/ui/Competitions';
import { ActivityList } from '@/ui/ActivityList';
import { useAuth } from '@/lib/auth';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { parseAsString, useQueryState } from 'nuqs';
import { cn } from '@/lib/cn';
import { RadioButtonGroup } from '@/ui/fields/RadioButtonGroupElement';
import { formatCount, formatPointsAndFinals } from '@/ui/format';
import { AgeHistogram, ClassHistogram } from '../../CohortComparison';
import { EntityLink } from '@/ui/EntityLink';

export function Cohort({
  initialCohort,
  initialMembers,
}: {
  initialCohort: CohortFragment;
  initialMembers: readonly CohortMemberFragment[];
}) {
  const [editingImage, setEditingImage] = React.useState(false);
  const [, updateImage] = useMutation(UpdateCohortDocument);
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );
  const [{ data }] = useQuery({
    query: CohortDocument,
    variables: {
      id: initialCohort.id,
      withMembers: true,
    },
  });
  const cohort = data?.cohort ?? initialCohort;
  const members = data?.cohort?.cohortMembershipsList ?? initialMembers;
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
        onEditImage={
          actions.some((action) => action.id === 'cohort.edit')
            ? () => setEditingImage(true)
            : undefined
        }
        title={cohort.name}
        breadcrumbLabel="Skupina"
        breadcrumbs={[{ title: 'Tréninkové skupiny', href: '/treninkove-skupiny' }]}
        subtitle={
          <DotSeparated>
            {cohort.location || undefined}
            {formatCount(members.length, { one: 'člen', few: 'členové', other: 'členů' })}
          </DotSeparated>
        }
        actions={actions}
      />


      <TabMenu selected={tab} onSelect={setTab}>
        <Tab id="members" title="Info">
          <RichTextView value={description} />
          <CohortMembers members={members} />
        </Tab>
        <Tab id="activity" title="Tréninky" requireMember>
          <ActivityList cohortId={cohort.id} />
        </Tab>
        <Tab id="fed" title="Soutěže" requireMember>
          <CompetitionHistory competitor={{ cohortId: cohort.id }} />
        </Tab>
        <Tab id="eligible" title="Možné soutěže" requireMember>
          <CompetitionSuggestions competitor={{ cohortId: cohort.id }} />
        </Tab>
        <Tab id="access" title="Přístupy" requireAdmin>
          <PeopleAccessOverview cohortId={cohort.id} />
        </Tab>
      </TabMenu>
    </>
  );
}

function CategoryList({
  person,
  discipline,
}: {
  person: CohortMemberFragment['person'];
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
      <div className="font-medium">{formatClass(item.category?.class)}</div>
      <div className="whitespace-nowrap text-sm text-neutral-11">
        {formatPointsAndFinals(item)}
      </div>
    </div>
  );
}

const CohortMembers = React.memo(function CohortMembers({
  members,
}: {
  members: readonly CohortMemberFragment[];
}) {
  const auth = useAuth();
  const [view, setView] = React.useState<'activity' | 'competitive'>(
    auth.isTrainer ? 'activity' : 'competitive',
  );
  const activityView = auth.isTrainer && view === 'activity';
  const people = members.flatMap((x) => x.person ?? []);
  const membershipActionMap = useActionMap(cohortMembershipActions, members);
  const memberActionMap = useActionMap(personActions, people);

  if (!auth.isMember || !auth.isTrainer) return null;

  return (
    <div
      className={cn(
        'grid gap-x-4 gap-y-3 pb-4',
        activityView
          ? 'grid-cols-1 min-[500px]:grid-cols-[minmax(0,1fr)_max-content]'
          : 'grid-cols-[minmax(0,1fr)_max-content_max-content] min-[500px]:grid-cols-[minmax(0,1fr)_max-content_max-content_max-content]',
      )}
    >
      {auth.isTrainer && (
        <div className="col-span-full justify-self-end">
          <RadioButtonGroup
            value={view}
            options={[
              { id: 'activity', label: 'Aktivita' },
              { id: 'competitive', label: 'Soutěžní info' },
            ]}
            onValueChange={(value) => setView(value as typeof view)}
          />
        </div>
      )}
      {!activityView && (
        <div className="col-span-full flex flex-wrap items-start gap-5">
          <AgeHistogram label="Věk" people={people} />
          <ClassHistogram label="STT" people={people} discipline="Standard" />
          <ClassHistogram label="LAT" people={people} discipline="Latin" />
        </div>
      )}
      {!activityView && (
        <div className="col-span-full grid grid-cols-subgrid items-center gap-x-5 gap-y-3 text-sm">
          <div
            className={cn(
              'mb-0 min-w-0',
              activityView ? 'col-span-1' : 'col-span-full min-[500px]:col-span-1',
            )}
          />
          <div />
          <div>STT</div>
          <div>LAT</div>
        </div>
      )}
      {members.map(({ id, person }) => (
        <div
          key={id}
          className="col-span-full grid grid-cols-subgrid items-center gap-x-5 gap-y-3 text-sm"
        >
          <ActionRow
            className={cn(
              'mb-0 min-w-0',
              activityView ? 'col-span-1' : 'col-span-full min-[500px]:col-span-1',
            )}
            actions={[
              ...(person ? memberActionMap.get(person.id)! : []),
              ...membershipActionMap.get(id)!,
            ]}
          >
            {person ? (
              <EntityLink type="person" entity={person} className="min-w-0 font-bold" />
            ) : (
              '?'
            )}
          </ActionRow>

          {activityView && person?.lastActivity && (
            <div className="min-w-0 lg:justify-self-end">
              <PersonActivityBadges activity={person.lastActivity} />
            </div>
          )}
          {!activityView && (
            <>
              <div className="font-medium text-right">
                {formatAgeGroup(person?.ageGroup)}
              </div>
              <div>
                <CategoryList person={person} discipline="Standard" />
              </div>
              <div>
                <CategoryList person={person} discipline="Latin" />
              </div>
            </>
          )}
        </div>
      ))}
    </div>
  );
});
