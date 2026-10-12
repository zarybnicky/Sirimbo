'use client';

import Link from 'next/link';
import { useEffect } from 'react';
import { parseAsInteger, useQueryState } from 'nuqs';
import { useQuery } from 'urql';
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
  const [currentPage, setPage] = useQueryState(
    'page',
    parseAsInteger
      .withDefault(1)
      .withOptions({ shallow: true, scroll: false, history: 'push' }),
  );
  const [{ data, fetching, error }] = useQuery({
    query: FederationEventCompetitionsDocument,
    variables: {
      federation,
      eventId,
      competitionsOffset: Math.min(Math.max(0, currentPage - 1) * 100, 2_147_483_647),
    },
  });
  const competitions = data?.federationEventByFederationAndId?.competitions;
  const total = competitions?.totalCount;
  useEffect(() => {
    if (fetching || error || total == null) return;
    const page = Math.max(1, Math.min(currentPage, Math.ceil(total / 100)));
    if (page !== currentPage) void setPage(page, { history: 'replace' });
  }, [currentPage, total, fetching, error, setPage]);
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
      <div className="col-full-width overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {[
                'competition ID',
                'category',
                'type',
                'status',
                'date',
                'check-in end',
                'registrations',
                'results',
              ].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {competitions.nodes.map((competition) => (
              <tr key={competition.id} className="border-t border-neutral-4">
                <td className="px-2 py-1">{competition.externalId}</td>
                <th scope="row" className="px-2 py-1 font-normal">
                  <Link
                    className="text-accent-11 underline"
                    href={`${path}/${competition.id}`}
                    prefetch={false}
                  >
                    {competition.category?.name || competition.externalId}
                  </Link>
                </th>
                <td className="px-2 py-1">
                  {competition.competitionType?.toLowerCase()}
                </td>
                <td className="px-2 py-1 whitespace-nowrap">
                  {competition.status.toLowerCase().replaceAll('_', ' ')}
                </td>
                <td className="px-2 py-1 whitespace-nowrap">
                  {formatDateOnly(competition.startDate)}
                </td>
                <td className="px-2 py-1">{competition.checkInEnd}</td>
                <td className="px-2 py-1">{competition.entries.totalCount}</td>
                <td className="px-2 py-1">{competition.results.totalCount}</td>
              </tr>
            ))}
            {competitions.nodes.length === 0 && (
              <tr>
                <td colSpan={8} className="px-2 py-1">
                  No competitions
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <Pagination
        total={competitions.totalCount}
        limit={100}
        page={currentPage}
        onPageChange={setPage}
      />
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
  const [currentPage, setPage] = useQueryState(
    'sourcesPage',
    parseAsInteger
      .withDefault(1)
      .withOptions({ shallow: true, scroll: false, history: 'push' }),
  );
  const [{ data, fetching, error }] = useQuery({
    query: CrawlerSourcesDocument,
    variables: {
      condition: { eventId },
      offset: Math.min(Math.max(0, currentPage - 1) * 100, 2_147_483_647),
    },
  });
  const sources = data?.crawlerTargets;
  const total = sources?.totalCount;
  useEffect(() => {
    if (fetching || error || total == null) return;
    const page = Math.max(1, Math.min(currentPage, Math.ceil(total / 100)));
    if (page !== currentPage) void setPage(page, { history: 'replace' });
  }, [currentPage, total, fetching, error, setPage]);

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
          <Pagination
            total={sources.totalCount}
            limit={100}
            page={currentPage}
            onPageChange={setPage}
          />
        </section>
      )}
    </>
  );
}
