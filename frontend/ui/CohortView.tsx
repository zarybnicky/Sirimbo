import { cohortActions } from '@/lib/actions/cohort';
import { cohortMembershipActions } from '@/lib/actions/cohortMembership';
import { useActionMap, useActions } from '@/lib/actions';
import type {
  CohortWithMembersQuery,
  CstsProgressRecordFragment,
} from '@/graphql/Cohorts';
import { RichTextView } from '@/ui/RichTextView';
import { PageHeader } from '@/ui/TitleBar';
import { formatCstsClass, getBestCstsProgress } from '@/ui/csts';
import { formatAgeGroup, formatOpenDateRange } from '@/ui/format';
import { typographyCls } from '@/ui/style';
import Link from 'next/link';
import React from 'react';
import { ActionRow } from '@/ui/ActionRow';
import { personActions } from '@/lib/actions/person';
import { isTruthy } from '@/lib/truthyFilter';

type CohortWithMembers = NonNullable<CohortWithMembersQuery['entity']>;

export function CohortView({ cohort }: { cohort: CohortWithMembers }) {
  const members = React.useMemo(
    () => cohort.cohortMembershipsList ?? [],
    [cohort.cohortMembershipsList],
  );
  const membershipActionMap = useActionMap(cohortMembershipActions, members);
  const memberActionMap = useActionMap(
    personActions,
    members.map((x) => x.person).filter(isTruthy),
  );
  const description = React.useMemo(
    () => cohort.description?.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', ''),
    [cohort.description],
  );
  const actions = useActions(cohortActions, cohort);

  return (
    <>
      <PageHeader title={cohort.name} actions={actions} />

      <h6 className="mb-2 font-bold">{cohort.location}</h6>
      <RichTextView value={description} />

      <h3 className={typographyCls({ variant: 'section', className: 'my-3' })}>
        Členové ({members.length})
      </h3>
      <div className="grid grid-cols-2 gap-x-4 gap-y-3 lg:grid-cols-[1fr_minmax(0,14rem)_minmax(0,14rem)_auto] pb-4">
        {members.map((membership) => (
          <div
            key={membership.id}
            className="col-span-full grid grid-cols-subgrid items-center gap-x-4 gap-y-2 text-sm"
          >
            <ActionRow
              className="mb-0 min-w-max"
              actions={[
                ...(membership.person ? memberActionMap.get(membership.person.id)! : []),
                ...membershipActionMap.get(membership.id)!,
              ]}
            >
              {membership.person ? (
                <Link
                  className="font-bold underline"
                  href={{
                    pathname: '/clenove/[id]',
                    query: { id: membership.person.id },
                  }}
                >
                  {membership.person.name}
                </Link>
              ) : (
                '?'
              )}
            </ActionRow>

            <div className="order-3 lg:order-2">
              <CategoryList person={membership.person} discipline="Standard" />
            </div>
            <div className="order-4 lg:order-3">
              <CategoryList person={membership.person} discipline="Latin" />
            </div>

            <div className="order-2 text-right lg:order-4">
              {formatOpenDateRange(membership)}
            </div>
          </div>
        ))}
      </div>
    </>
  );
}

function CategoryList({
  person,
  discipline,
}: {
  person?: {
    birthDate?: string | null;
    cstsProgressList?: CstsProgressRecordFragment[] | null;
  } | null;
  discipline: string;
}) {
  if (!person?.cstsProgressList) return null;
  const item = getBestCstsProgress(person.cstsProgressList, discipline);
  console.log(person.cstsProgressList, item);
  if (!item) return null;

  return (
    <div className="flex flex-col leading-tight text-center">
      <div className="font-medium">
        {[
          formatAgeGroup(person.birthDate),
          formatCstsClass(item.category?.class),
          item.category?.discipline === 'Standard'
            ? 'STT'
            : item.category?.discipline === 'Latin'
              ? 'LAT'
              : item.category?.discipline,
        ]
          .filter(Boolean)
          .join(' ')}
      </div>
      <div className="text-xs text-neutral-11">
        {Number.parseFloat(item.points ?? '0')} / {item.finals}F
      </div>
    </div>
  );
}
