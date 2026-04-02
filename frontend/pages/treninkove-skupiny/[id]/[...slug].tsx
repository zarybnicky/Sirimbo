import { Layout } from '@/ui/Layout';
import { CohortWithMembersDocument } from '@/graphql/Cohorts';
import { CohortList } from '@/ui/lists/CohortList';
import { RichTextView } from '@/ui/RichTextView';
import { PageHeader } from '@/ui/TitleBar';
import { WithSidebar } from '@/ui/WithSidebar';
import { slugify } from '@/lib/slugify';
import { typographyCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { useTypedRouter, zRouterString } from '@/ui/useTypedRouter';
import React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';
import Link from 'next/link';
import { formatOpenDateRange } from '@/ui/format';
import { Spinner } from '@/ui/Spinner';
import { useActionMap, useActions } from '@/lib/actions';
import { cohortActions } from '@/lib/actions/cohort';
import { cohortMembershipActions } from '@/lib/actions/cohortMembership';
import { ActionGroup } from '@/ui/ActionGroup';

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
  const memberActionMap = useActionMap(cohortMembershipActions, members);
  const description = React.useMemo(
    () => cohort?.description?.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', ''),
    [cohort?.description],
  );
  const actions = useActions(cohortActions, cohort);

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
        <PageHeader title={cohort.name} actions={actions} />

        <h6 className="font-bold mb-2">{cohort.location}</h6>
        <RichTextView value={description} />

        <h3 className={typographyCls({ variant: 'section', className: 'my-3' })}>
          Členové ({members.length})
        </h3>
        <div className="grid grid-cols-2 gap-x-2 gap-y-3 lg:grid-cols-[1fr_fit-content(30%)_fit-content(30%)_1fr]">
          <div className="col-span-full grid-cols-subgrid gap-2 text-sm hidden lg:grid">
            <div></div>
            <div>STT</div>
            <div>LAT</div>
            <div></div>
          </div>
          {members.map((membership) => (
            <div
              key={membership.id}
              className="grid col-span-full grid-cols-subgrid gap-2 text-sm items-center"
            >
              <div className="flex align-baseline gap-2">
                {auth.isAdmin && (
                  <ActionGroup
                    variant="row"
                    actions={memberActionMap.get(membership.id)!}
                  />
                )}
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
              </div>
              <div className="flex lg:flex-col order-3 lg:order-2 gap-1">
                {(membership.person?.cstsProgressList ?? []).some(
                  (x) => x.category?.discipline === 'Standard',
                ) ? (
                  <div className="lg:hidden">STT:</div>
                ) : (
                  <></>
                )}
                {(membership.person?.cstsProgressList ?? [])
                  .filter((x) => x.category?.discipline === 'Standard')
                  .map(({ category, points, finals }, i) => (
                    <div key={i}>
                      {category
                        ? `${category.class === 'S' ? 'M' : category.class} ${Number.parseFloat(points ?? '0')}/${finals}F`
                        : ''}
                    </div>
                  ))}
              </div>
              <div className="flex lg:flex-col order-4 lg:order-3 gap-1">
                {(membership.person?.cstsProgressList ?? []).some(
                  (x) => x.category?.discipline === 'Latin',
                ) ? (
                  <div className="lg:hidden">LAT:</div>
                ) : (
                  <></>
                )}
                {(membership.person?.cstsProgressList ?? [])
                  .filter((x) => x.category?.discipline === 'Latin')
                  .map(({ category, points, finals }, i) => (
                    <div key={i}>
                      {category
                        ? `${category.class === 'S' ? 'M' : category.class} ${Number.parseFloat(points ?? '0')}/${finals}F`
                        : ''}
                    </div>
                  ))}
              </div>
              <div className="order-2 lg:order-4 text-right">
                {formatOpenDateRange(membership)}
              </div>
            </div>
          ))}
        </div>
      </WithSidebar>
    </Layout>
  );
}

export default TrainingCohortPage;
