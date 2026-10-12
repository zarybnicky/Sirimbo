'use client';

import Link from 'next/link';
import { cn } from '@/lib/cn';
import { useEffect } from 'react';
import { parseAsInteger, useQueryState } from 'nuqs';
import { useQuery } from 'urql';
import { FormError } from '@/ui/form';
import { Spinner } from '@/ui/Spinner';
import { Pagination } from '@/ui/Pagination';
import {
  FederationCompetitorMembersDocument,
  FederationCompetitorHistoryDocument,
} from '@/graphql/Federation';
import { formatDateOnly, formatResultStatus } from '@/ui/format';

export function Members({ federation, id }: { federation: string; id: string }) {
  const [currentPage, setPage] = useQueryState(
    'membersPage',
    parseAsInteger
      .withDefault(1)
      .withOptions({ shallow: true, scroll: false, history: 'push' }),
  );
  const [{ data, fetching, error }] = useQuery({
    query: FederationCompetitorMembersDocument,
    variables: {
      id,
      membersOffset: Math.min(Math.max(0, currentPage - 1) * 100, 2_147_483_647),
    },
  });
  const members = data?.federationCompetitor?.components;
  const total = members?.totalCount;
  useEffect(() => {
    if (fetching || error || total == null) return;
    const page = Math.max(1, Math.min(currentPage, Math.ceil(total / 100)));
    if (page !== currentPage) void setPage(page, { history: 'replace' });
  }, [currentPage, total, fetching, error, setPage]);
  const base = `/federated/${federation}`;
  if (error) return <FormError error={error} />;
  if (!members) return <Spinner />;

  return (
    <>
      {fetching && (
        <p role="status" className="my-2 text-sm text-neutral-11">
          Loading…
        </p>
      )}
      <h2 className="mt-4 mb-2 text-lg font-bold">Members</h2>
      <div className="overflow-x-auto">
        <table className="w-full text-left text-sm">
          <thead>
            <tr>
              {['person', 'role'].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {members.nodes.map(({ personId, person, role }) => (
              <tr key={personId} className="border-t border-neutral-4">
                <th scope="row" className="px-2 py-1 font-normal">
                  {person ? (
                    <Link
                      className="text-accent-11 underline"
                      href={`${base}/people/${person.externalId}`}
                    >
                      {person.canonicalName ||
                        [person.firstName, person.lastName].filter(Boolean).join(' ') ||
                        personId}
                    </Link>
                  ) : (
                    personId
                  )}
                </th>
                <td className="px-2 py-1">{role.toLowerCase()}</td>
              </tr>
            ))}
            {members.nodes.length === 0 && (
              <tr>
                <td colSpan={2} className="px-2 py-1">
                  No members
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <Pagination
        total={members.totalCount}
        limit={100}
        page={currentPage}
        onPageChange={setPage}
      />
    </>
  );
}

export function History({ federation, id }: { federation: string; id: string }) {
  const [currentPage, setPage] = useQueryState(
    'page',
    parseAsInteger
      .withDefault(1)
      .withOptions({ shallow: true, scroll: false, history: 'push' }),
  );
  const [{ data, fetching, error }] = useQuery({
    query: FederationCompetitorHistoryDocument,
    variables: {
      id,
      historyOffset: Math.min(Math.max(0, currentPage - 1) * 50, 2_147_483_647),
    },
  });
  const history = data?.competitions;
  const total = history?.totalCount;
  useEffect(() => {
    if (fetching || error || total == null) return;
    const page = Math.max(1, Math.min(currentPage, Math.ceil(total / 50)));
    if (page !== currentPage) void setPage(page, { history: 'replace' });
  }, [currentPage, total, fetching, error, setPage]);
  const base = `/federated/${federation}`;
  if (error) return <FormError error={error} />;
  if (!history) return <Spinner />;
  const showPoints = history.nodes.some((row) =>
    row.results.nodes.some((result) => result.pointGain != null),
  );
  const showFinalGain = history.nodes.some((row) =>
    row.results.nodes.some((result) =>
      federation === 'csts' ? result.isFinal : result.finalGain != null,
    ),
  );

  return (
    <>
      {fetching && (
        <p role="status" className="my-2 text-sm text-neutral-11">
          Loading…
        </p>
      )}
      <h2 className="mt-4 mb-2 text-lg font-bold">Registrations and results</h2>
      <div className="col-full-width overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {['date', 'event', 'competition', 'status', 'registration', 'rank'].map(
                (label) => (
                  <th key={label} scope="col" className="px-2 py-1">
                    {label}
                  </th>
                ),
              )}
              {showPoints && (
                <th scope="col" className="px-2 py-1">
                  points
                </th>
              )}
              {showFinalGain && (
                <th scope="col" className="px-2 py-1">
                  final gain
                </th>
              )}
            </tr>
          </thead>
          <tbody>
            {history.nodes.map((row) => {
              const entry = row.entries.nodes[0];
              const result = row.results.nodes[0];
              const status = formatResultStatus(result?.completionStatus);
              return (
                <tr key={row.id} className="border-t border-neutral-4">
                  <td className="px-2 py-1 whitespace-nowrap">
                    {formatDateOnly(row.startDate)}
                  </td>
                  <td className="px-2 py-1">
                    {row.event && (
                      <Link
                        className="text-accent-11 underline"
                        href={`${base}/${row.event.externalId}`}
                        prefetch={false}
                      >
                        {row.event.name || row.event.externalId}
                      </Link>
                    )}
                  </td>
                  <th scope="row" className="px-2 py-1 font-normal">
                    {row.event ? (
                      <Link
                        className={cn(
                          'text-accent-11',
                          row.status === 'CANCELLED' ? 'line-through' : 'underline',
                        )}
                        href={`${base}/${row.event.externalId}/${row.externalId}`}
                        prefetch={false}
                      >
                        {row.category?.name || row.externalId}
                      </Link>
                    ) : (
                      row.category?.name || row.externalId
                    )}
                    {status && <div className="text-xs text-neutral-11">{status}</div>}
                  </th>
                  <td className="px-2 py-1 whitespace-nowrap">
                    {row.status.toLowerCase().replaceAll('_', ' ')}
                  </td>
                  <td className="px-2 py-1">
                    {entry?.cancelled
                      ? 'cancelled'
                      : !result && entry
                        ? 'registered'
                        : ''}
                  </td>
                  <td className="px-2 py-1 whitespace-nowrap">
                    {result?.ranking}
                    {result?.rankingTo && result?.rankingTo !== result?.ranking
                      ? `–${result?.rankingTo}`
                      : ''}
                  </td>
                  {showPoints && (
                    <td className="px-2 py-1">
                      {result?.pointGain == null ? null : Number(result.pointGain)}
                    </td>
                  )}
                  {showFinalGain && (
                    <td className="px-2 py-1">
                      {federation === 'csts'
                        ? result?.isFinal
                          ? 'F'
                          : ''
                        : result?.finalGain == null
                          ? null
                          : Number(result.finalGain)}
                    </td>
                  )}
                </tr>
              );
            })}
            {history.nodes.length === 0 && (
              <tr>
                <td
                  colSpan={6 + Number(showPoints) + Number(showFinalGain)}
                  className="px-2 py-1"
                >
                  No registrations or results
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <Pagination
        total={history.totalCount}
        limit={50}
        page={currentPage}
        onPageChange={setPage}
      />
    </>
  );
}
