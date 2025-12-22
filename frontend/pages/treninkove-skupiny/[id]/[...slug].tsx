import { Layout } from '@/ui/Layout';
import {
  CohortWithMembersDocument,
  type CohortWithMembersQuery,
} from '@/graphql/Cohorts';
import { CohortList } from '@/ui/lists/CohortList';
import { RichTextView } from '@/ui/RichTextView';
import { TitleBar } from '@/ui/TitleBar';
import { WithSidebar } from '@/ui/WithSidebar';
import { CohortForm } from '@/ui/forms/CohortForm';
import { slugify } from '@/ui/slugify';
import { typographyCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { useTypedRouter, zRouterString } from '@/ui/useTypedRouter';
import React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { buttonCls } from '@/ui/style';
import { exportCohort } from '@/ui/reports/export-cohort';
import { DropdownMenuTrigger } from '@/ui/dropdown';
import { CohortMembershipMenu } from '@/ui/menus/CohortMembershipMenu';
import Link from 'next/link';
import { formatOpenDateRange } from '@/ui/format';
import { Spinner } from '@/ui/Spinner';

const QueryParams = z.object({
  id: zRouterString,
  slug: zRouterString,
});

function TrainingCohortPage() {
  const auth = useAuth();
  const router = useTypedRouter(QueryParams);
  const idParam = router.query.id || router.query.slug;
  const [{ data: cohortQuery, fetching: fetchingCohort }] = useQuery({
    query: CohortWithMembersDocument,
    variables: { id: idParam || '0' },
    pause: !router.isReady || !idParam,
  });
  const cohort = cohortQuery?.entity;
  const members = React.useMemo(
    () => cohort?.cohortMembershipsList ?? [],
    [cohort?.cohortMembershipsList],
  );
  const description = React.useMemo(
    () => cohort?.description?.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', ''),
    [cohort?.description],
  );

  React.useEffect(() => {
    if (!router.isReady || !idParam || fetchingCohort) return;
    if (!cohort) {
      void router.replace('/404');
      return;
    }
    const expectedSlug = slugify(cohort.name);
    if (expectedSlug && router.query.slug !== expectedSlug) {
      void router.replace({
        pathname: '/treninkove-skupiny/[id]/[...slug]',
        query: {
          id: cohort.id,
          slug: [expectedSlug],
        },
      });
    }
  }, [cohort, fetchingCohort, idParam, router]);

  if (!cohort) {
    return (
      <Layout hideTopMenuIfLoggedIn>
        <WithSidebar sidebar={<CohortList />}>
          <div className="flex justify-center py-10">
            <Spinner />
          </div>
        </WithSidebar>
      </Layout>
    );
  }

  return (
    <Layout hideTopMenuIfLoggedIn>
      <WithSidebar sidebar={<CohortList />}>
        <TitleBar title={cohort?.name}>
          {auth.isTrainerOrAdmin && cohort.id && (
            <button
              type="button"
              className={buttonCls({ size: 'sm', variant: 'outline' })}
              onClick={() => exportCohort([cohort.id], cohort.name)}
            >
              Export členů
            </button>
          )}
          {auth.isAdmin && cohort.id && (
            <Dialog>
              <DialogTrigger.Edit size="sm" />
              <DialogContent>
                <CohortForm id={cohort.id} />
              </DialogContent>
            </Dialog>
          )}
        </TitleBar>

        <h6 className="font-bold mb-2">{cohort.location}</h6>
        <RichTextView value={description} />

        <h3 className={typographyCls({ variant: 'section', className: 'my-3' })}>
          Členové ({members.length})
        </h3>
        {members.map((membership) => (
          <div key={membership.id} className="flex gap-3 mb-1 align-baseline">
            {auth.isAdmin && (
              <CohortMembershipMenu data={membership}>
                <DropdownMenuTrigger.RowDots />
              </CohortMembershipMenu>
            )}
            <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
              {membership.person ? (
                <Link
                  className="underline font-bold"
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
              <div className="flex gap-1">
                {(membership.person?.cstsProgressList ?? []).map(
                  ({ category, points, finals }, i) => (
                    <div key={i}>
                      {category
                        ? `${
                            category.discipline === 'Standard'
                              ? 'STT'
                              : category.discipline === 'Latin'
                                ? 'LAT'
                                : category.discipline
                          } ${category.class} ${Number.parseFloat(points ?? '0')}/${finals}F`
                        : ''}
                    </div>
                  ),
                )}
              </div>
              <span>{formatOpenDateRange(membership)}</span>
            </div>
          </div>
        ))}
      </WithSidebar>
    </Layout>
  );
}

export default TrainingCohortPage;
