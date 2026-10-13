'use client';

import { Table } from '@/ui/Table';
import Link from 'next/link';
import { cn } from '@/lib/cn';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { FormError } from '@/ui/form';
import { Spinner } from '@/ui/Spinner';
import { Pagination } from '@/ui/Pagination';
import { FederationEventCompetitionsDocument } from '@/graphql/Federation';
import { CrawlerSourcesDocument } from '@/graphql/Crawler';
import { formatDateOnly } from '@/ui/format';

export function Competitions({
  federation,
  eventId,
}: {
  federation: string;
  eventId: string;
}) {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: FederationEventCompetitionsDocument,
    variables: (offset) => ({
      federation,
      eventId,
      competitionsOffset: offset,
    }),
    limit: 100,
    getTotal: (data) =>
      data?.federationEventByFederationAndExternalId?.competitions?.totalCount,
  });
  const competitions = data?.federationEventByFederationAndExternalId?.competitions;
  const path = `/federated/${federation}/${eventId}`;
  if (error) return <FormError error={error} />;
  if (!competitions) return <Spinner />;

  return (
    <>
      {fetching && (
        <p role="status" className="my-2 text-sm text-neutral-11">
          Loading…
        </p>
      )}
      <h2 className="mt-4 mb-2 text-lg font-bold">Competitions</h2>
      <Table
        columns={[
          'category',
          'type',
          'status',
          'date',
          'check-in end',
          'registrations',
          'results',
        ]}
        className="col-full-width"
        empty="No competitions"
      >
        {competitions.nodes.map((competition) => (
          <tr key={competition.id} className="border-t border-neutral-4">
            <th scope="row">
              <Link
                className={cn(
                  'text-accent-11',
                  competition.status === 'CANCELLED' ? 'line-through' : 'underline',
                )}
                href={`${path}/${competition.externalId}`}
                prefetch={false}
              >
                {competition.category?.name || competition.externalId}
              </Link>
            </th>
            <td>{competition.competitionType?.toLowerCase().replaceAll('_', ' ')}</td>
            <td className="whitespace-nowrap">
              {competition.status.toLowerCase().replaceAll('_', ' ')}
            </td>
            <td className="whitespace-nowrap">{formatDateOnly(competition.startDate)}</td>
            <td>{competition.checkInEnd?.slice(0, 5)}</td>
            <td>{competition.entries.totalCount}</td>
            <td>{competition.results.totalCount}</td>
          </tr>
        ))}
      </Table>
      <Pagination {...pagination} />
    </>
  );
}

export function Sources({
  federation,
  eventId,
}: {
  federation: string;
  eventId: string;
}) {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: CrawlerSourcesDocument,
    variables: (offset) => ({
      condition: { eventId },
      offset,
    }),
    limit: 100,
    pageKey: 'sourcesPage',
    getTotal: (data) => data?.crawlerTargets?.totalCount,
  });
  const sources = data?.crawlerTargets;

  if (error) return <FormError error={error} />;
  if (!sources) return <Spinner />;

  return (
    <>
      {fetching && (
        <p role="status" className="my-2 text-sm text-neutral-11">
          Loading…
        </p>
      )}
      {sources.totalCount > 0 && (
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
          <Pagination {...pagination} />
        </section>
      )}
    </>
  );
}
