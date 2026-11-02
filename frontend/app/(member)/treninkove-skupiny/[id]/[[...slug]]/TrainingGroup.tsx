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
import {
  ageGroupNames,
  formatAgeGroup,
  formatClass,
  getBestCstsProgress,
} from '@/ui/csts';
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
import { ActivityList } from '@/ui/ActivityList';
import { useAuth } from '@/lib/auth';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { parseAsString, useQueryState } from 'nuqs';
import { cn } from '@/lib/cn';
import { RadioButtonGroup } from '@/ui/fields/RadioButtonGroupElement';
import { formatCount, formatPointsAndFinals } from '@/ui/format';
import { Histogram } from '@/ui/Histogram';
import { rollup } from 'd3';

const shortAgeGroups: Record<string, string> = {
  Under_8: 'U8',
  Juvenile: 'D',
  Juvenile_I: 'D1',
  Juvenile_II: 'D2',
  Junior: 'J',
  Junior_I: 'J1',
  Junior_II: 'J2',
  Youth: 'Ml',
  Under_21: 'U21',
  Adult: 'Do',
  Senior: 'S',
  Senior_I: 'S1',
  Senior_II: 'S2',
  Senior_III: 'S3',
  Senior_IV: 'S4',
  Senior_V: 'S5',
};

const collapsedAgeGroups: Record<string, string> = {
  Senior_Ia: 'Adult',
  Senior_Ib: 'Senior_I',
  Senior_IIa: 'Senior_I',
  Senior_IIb: 'Senior_II',
  Senior_IIIa: 'Senior_II',
  Senior_IIIb: 'Senior_III',
  Senior_IVa: 'Senior_III',
  Senior_IVb: 'Senior_IV',
};

const ageGroupOrder = new Map(
  Object.keys(ageGroupNames).map((code, index) => [code, index]),
);

export function TrainingGroup({
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

      <TabMenu selected={tab} onSelect={setTab}>
        <Tab id="members" title={`Členové (${members.length})`} requireMember>
          <TrainingGroupMembers members={members} />
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

const TrainingGroupMembers = React.memo(function TrainingGroupMembers({
  members,
}: {
  members: readonly CohortMemberFragment[];
}) {
  const auth = useAuth();
  const [view, setView] = React.useState<'activity' | 'competitive'>(
    auth.isTrainer ? 'activity' : 'competitive',
  );
  const activityView = auth.isTrainer && view === 'activity';
  const people = members.map((member) => member.person).filter(isTruthy);
  const ages = rollup(
    people,
    (group) => group.length,
    (person) => collapsedAgeGroups[person.ageGroup ?? ''] ?? person.ageGroup ?? '?',
  );
  const ageCounts = [
    ...new Set([
      '?',
      ...Object.keys(shortAgeGroups).filter(
        (code) => !['Juvenile', 'Junior', 'Senior'].includes(code),
      ),
      ...ages.keys(),
    ]),
  ]
    .filter(
      (code) =>
        !['Under_8', 'Juvenile_I', 'Juvenile_II'].includes(code) || ages.has(code),
    )
    .toSorted(
      (a, b) =>
        Number(b === '?') - Number(a === '?') ||
        (ageGroupOrder.get(a) ?? Infinity) - (ageGroupOrder.get(b) ?? Infinity) ||
        a.localeCompare(b),
    )
    .map((code): [string, number] => [code, ages.get(code) ?? 0]);
  const classCounts = (discipline: string) => {
    const counts = rollup(
      people,
      (group) => group.length,
      (person) =>
        (person.cstsId &&
          formatClass(
            getBestCstsProgress(person.federationCategoryProgresses.nodes, discipline)
              ?.category?.class,
          )) ||
        '?',
    );
    return ['?', 'E', 'D', 'C', 'B', 'A', 'M'].map((code): [string, number] => [
      code,
      counts.get(code) ?? 0,
    ]);
  };
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
          <Histogram
            label="Věk"
            data={ageCounts}
            formatLabel={(value) => shortAgeGroups[value] ?? value}
            formatTitle={(value) => formatAgeGroup(value) ?? '?'}
          />
          <Histogram label="STT" data={classCounts('Standard')} bucketWidth={14} />
          <Histogram label="LAT" data={classCounts('Latin')} bucketWidth={14} />
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
              <Link
                className="min-w-0 font-bold underline"
                href={`/clenove/${person.id}`}
              >
                {person.name}
              </Link>
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
