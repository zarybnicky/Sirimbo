'use client';

import Link from 'next/link';
import type { CrawlerTargetCondition } from '@/graphql';
import { CrawlerSourcesDocument } from '@/graphql/Crawler';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { FormError } from '@/ui/form';
import { Pagination } from '@/ui/Pagination';
import { Spinner } from '@/ui/Spinner';
import { useAuth } from '@/lib/auth';

export function Sources({
  federation,
  condition,
}: {
  federation: string;
  condition: CrawlerTargetCondition;
}) {
  const auth = useAuth();

  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: CrawlerSourcesDocument,
    variables: (offset) => ({ condition, offset }),
    limit: 100,
    pageKey: 'sourcesPage',
    getTotal: (data) => data?.crawlerTargets?.totalCount,
    pause: !auth.isSystemAdmin,
  });

  if (!auth.isSystemAdmin) return null;
  if (error) return <FormError error={error} />;
  if (!data || fetching) return <Spinner />;

  const sources = data.crawlerTargets?.nodes || [];
  if (sources.length === 0) return null;

  return (
    <section>
      <h2 className="mt-4 mb-2 text-lg font-bold">Crawler</h2>
      {sources.map((source) => (
        <p key={source.id}>
          <Link
            className="text-accent-11 underline"
            href={`/admin/crawler/${source.id}`}
            prefetch={false}
          >
            {federation}:{source.kind}:{source.key}
          </Link>{' '}
          (fetch: {source.fetchStatus.toLowerCase()}, process:{' '}
          {source.processStatus.toLowerCase()})
        </p>
      ))}
      {sources.length === 0 && <p>No matching frontiers</p>}
      <Pagination {...pagination} />
    </section>
  );
}
