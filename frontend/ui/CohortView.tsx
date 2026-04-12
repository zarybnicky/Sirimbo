import { cohortActions } from '@/lib/actions/cohort';
import { cohortMembershipActions } from '@/lib/actions/cohortMembership';
import { useActionMap, useActions } from '@/lib/actions';
import type { CohortWithMembersQuery } from '@/graphql/Cohorts';
import { ActionGroup } from '@/ui/ActionGroup';
import { RichTextView } from '@/ui/RichTextView';
import { PageHeader } from '@/ui/TitleBar';
import { getBestCstsProgress, normalizeCstsClass } from '@/ui/csts';
import { formatAgeGroup, formatOpenDateRange } from '@/ui/format';
import { typographyCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import Link from 'next/link';
import React from 'react';

type CohortWithMembers = NonNullable<CohortWithMembersQuery['entity']>;
type CohortMembership = CohortWithMembers['cohortMembershipsList'][number];
type CstsProgressRecord = NonNullable<
  NonNullable<CohortMembership['person']>['cstsProgressList']
>[number];

export function CohortView({ cohort }: { cohort: CohortWithMembers }) {
  const auth = useAuth();
  const members = React.useMemo(
    () => cohort.cohortMembershipsList ?? [],
    [cohort.cohortMembershipsList],
  );
  const memberActionMap = useActionMap(cohortMembershipActions, members);
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
            <div className="flex items-center gap-2 min-w-max">
              {auth.isAdmin && (
                <ActionGroup
                  variant="row"
                  actions={memberActionMap.get(membership.id)!}
                />
              )}
              <div>
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
              </div>
            </div>

            <div className="order-3 lg:order-2">
              <CategoryList
                birthDate={membership.person?.birthDate}
                progressList={(membership.person?.cstsProgressList || [])?.filter(
                  (x) => x.category?.discipline === 'Standard',
                )}
              />
            </div>

            <div className="order-4 lg:order-3">
              <CategoryList
                birthDate={membership.person?.birthDate}
                progressList={(membership.person?.cstsProgressList || [])?.filter(
                  (x) => x.category?.discipline === 'Latin',
                )}
              />
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
  birthDate,
  progressList,
}: {
  birthDate: string | null | undefined;
  progressList: CstsProgressRecord[];
}) {
  const item = getBestCstsProgress(progressList);

  if (!item) {
    return null;
  }

  return (
    <div className="flex flex-col gap-2">
      <div className="leading-tight text-center">
        <div className="font-medium">
          {[
            formatAgeGroup(birthDate) ?? item.category.ageGroup,
            normalizeCstsClass(item.category.class),
            item.category.discipline === 'Standard'
              ? 'STT'
              : item.category.discipline === 'Latin'
                ? 'LAT'
                : item.category.discipline,
          ]
            .filter(Boolean)
            .join(' ')}
        </div>
        <div className="text-xs text-neutral-11">
          {Number.parseFloat(item.points ?? '0')} / {item.finals}F
        </div>
      </div>
    </div>
  );
}
