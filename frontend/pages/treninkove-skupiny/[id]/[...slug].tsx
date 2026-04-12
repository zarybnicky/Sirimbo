import { Layout } from '@/ui/Layout';
import { CohortWithMembersDocument } from '@/graphql/Cohorts';
import { CohortList } from '@/ui/lists/CohortList';
import { WithSidebar } from '@/ui/WithSidebar';
import { slugify } from '@/lib/slugify';
import { useTypedRouter, zRouterString } from '@/ui/useTypedRouter';
import { CohortView } from '@/ui/CohortView';
import React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';
import { Spinner } from '@/ui/Spinner';

const QueryParams = z.object({
  id: zRouterString,
  slug: zRouterString,
});

function TrainingCohortPage() {
  const router = useTypedRouter(QueryParams);
  const idParam = router.query.id || router.query.slug;
  const [{ data: cohortQuery, fetching: fetchingCohort }] = useQuery({
    query: CohortWithMembersDocument,
    variables: { id: idParam || '0' },
    pause: !router.isReady || !idParam,
  });
  const cohort = cohortQuery?.entity;

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
        <CohortView cohort={cohort} />
      </WithSidebar>
    </Layout>
  );
}

export default TrainingCohortPage;
