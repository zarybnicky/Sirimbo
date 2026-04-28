import { CohortGroupDocument } from '@/graphql/CohortGroup';
import { PageHeader } from '@/ui/TitleBar';
import { RichTextView } from '@/ui/RichTextView';
import { slugify } from '@/lib/slugify';
import { Layout } from '@/ui/Layout';
import React from 'react';
import Link from 'next/link';
import { z } from 'zod';
import { useTypedRouter, zRouterString } from '@/ui/useTypedRouter';
import { cardCls } from '@/ui/style';
import { useQuery } from 'urql';
import { Spinner } from '@/ui/Spinner';
import { useActionMap } from '@/lib/actions';
import { cohortActions } from '@/lib/actions/cohort';
import { ActionGroup } from '@/ui/ActionGroup';

const QueryParams = z.object({
  id: zRouterString,
  slug: zRouterString,
});

function TrainingGroupPage() {
  const router = useTypedRouter(QueryParams);
  const idParam = router.query.id || router.query.slug;
  const [{ data, fetching }] = useQuery({
    query: CohortGroupDocument,
    variables: { id: idParam || '0' },
    pause: !router.isReady || !idParam,
  });
  const item = data?.cohortGroup;
  const cohortActionMap = useActionMap(cohortActions, item?.cohortsList ?? []);

  React.useEffect(() => {
    if (!router.isReady || !idParam || fetching) return;
    if (!item) {
      void router.replace('/404');
      return;
    }
    const expectedSlug = slugify(item.name);
    if (expectedSlug && router.query.slug !== expectedSlug) {
      void router.replace({
        pathname: '/treninkove-programy/[id]/[...slug]',
        query: {
          id: item.id,
          slug: [expectedSlug],
        },
      });
    }
  }, [fetching, idParam, item, router]);

  if (!item) {
    return (
      <Layout hideTopMenuIfLoggedIn>
        <div className="flex justify-center py-10">
          <Spinner />
        </div>
      </Layout>
    );
  }

  return (
    <Layout hideTopMenuIfLoggedIn>
      <PageHeader title={item.name} />
      <div className="container py-4">
        <RichTextView className="mb-10" value={item.description} />
        {item.cohortsList.map((item) => (
          <div
            key={item.id}
            className={cardCls({ className: 'group break-inside-avoid pl-8' })}
          >
            <div className="mb-2 flex items-start justify-between gap-3">
              <h5 className="text-xl underline">
                <Link
                  href={{
                    pathname: '/treninkove-skupiny/[id]/[...slug]',
                    query: { id: item.id, slug: [slugify(item.name)] },
                  }}
                >
                  {item.name}
                </Link>
              </h5>
              <ActionGroup actions={cohortActionMap.get(item.id)!} />
            </div>
            <h6 className="font-bold mb-2">{item.location}</h6>
            <RichTextView
              value={item.description.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')}
            />
            <div
              className="absolute rounded-l-lg w-4 border-r border-neutral-6 shadow-sm inset-y-0 left-0"
              style={{ backgroundColor: item.colorRgb }}
            />
          </div>
        ))}
      </div>
    </Layout>
  );
}

export default TrainingGroupPage;
