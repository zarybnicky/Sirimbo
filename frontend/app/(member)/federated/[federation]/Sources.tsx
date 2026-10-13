'use client';

import Link from 'next/link';
import type { CrawlerTargetCondition } from '@/graphql';
import { CrawlerSourcesDocument } from '@/graphql/Crawler';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { FormError } from '@/ui/form';
import { Pagination } from '@/ui/Pagination';
import { Spinner } from '@/ui/Spinner';

export function Sources({
  federation,
  condition,
  hideEmpty = false,
}: {
  federation: string;
  condition: CrawlerTargetCondition;
  hideEmpty?: boolean;
}) {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: CrawlerSourcesDocument,
    variables: (offset) => ({
      condition,
      offset,
    }),
    limit: 100,
    pageKey: 'sourcesPage',
    getTotal: (data) => data?.crawlerTargets?.totalCount,
  });
  const sources = data?.crawlerTargets;

  if (error) return <FormError error={error} />;
  if (!sources) return <Spinner />;
  if (hideEmpty && sources.totalCount === 0) return null;

  return (
    <>
      {fetching && (
        <p role="status" className="my-2 text-sm text-neutral-11">
          Loading…
        </p>
      )}
      <section>
        <h2 className="mt-4 mb-2 text-lg font-bold">Crawler</h2>
        {sources.nodes.map((source) => (
          <p key={source.id}>
            <Link
              className="text-accent-11 underline"
              href={`/admin/crawler/${source.id}`}
              prefetch={false}
            >
              {federation}:{source.kind}:{source.key}
            </Link>{' '}
            — fetch: {source.fetchStatus.toLowerCase()}, process:{' '}
            {source.processStatus.toLowerCase()}
          </p>
        ))}
        {sources.totalCount === 0 && <p>No matching frontiers</p>}
        <Pagination {...pagination} />
      </section>
    </>
  );
}
