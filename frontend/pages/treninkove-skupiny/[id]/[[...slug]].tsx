import { Layout } from '@/ui/Layout';
import { CohortWithMembersDocument } from '@/graphql/Cohorts';
import { CohortList } from '@/ui/lists/CohortList';
import { WithSidebar } from '@/ui/WithSidebar';
import { slugify } from '@/lib/slugify';
import { useTypedRouter, zRouterString } from '@/ui/useTypedRouter';
import { CohortView } from '@/ui/CohortView';
import { useRouter as useCompatRouter } from 'next/compat/router';
import React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';
import { Spinner } from '@/ui/Spinner';
import { NextSeo } from 'next-seo';

const QueryParams = z.object({
  id: zRouterString,
  slug: zRouterString,
});

function TrainingCohortPage() {
  const router = useTypedRouter(QueryParams);
  const compatRouter = useCompatRouter();
  const idParam = router.query.id || router.query.slug;
  const replace = React.useCallback(
    (href: string) => {
      if (compatRouter) {
        void compatRouter.replace(href);
      } else {
        window.location.replace(href);
      }
    },
    [compatRouter],
  );
  const [{ data: cohortQuery, fetching: fetchingCohort }] = useQuery({
    query: CohortWithMembersDocument,
    variables: { id: idParam || '0' },
    pause: !idParam,
  });
  const cohort = cohortQuery?.entity;

  React.useEffect(() => {
    if (!idParam || fetchingCohort) return;
    if (!cohort) {
      replace('/404');
      return;
    }
    const expectedSlug = slugify(cohort.name);
    if (expectedSlug && router.query.slug !== expectedSlug) {
      replace(`/treninkove-skupiny/${cohort.id}/${expectedSlug}`);
    }
  }, [cohort, fetchingCohort, idParam, replace, router.query.slug]);

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
      <NextSeo title={cohort.name} />
      <WithSidebar sidebar={<CohortList />}>
        <CohortView cohort={cohort} />
      </WithSidebar>
    </Layout>
  );
}

export default TrainingCohortPage;
