'use client';

import { Table } from '@/ui/Table';
import Link from 'next/link';
import { cn } from '@/lib/cn';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { FormError } from '@/ui/form';
import { Spinner } from '@/ui/Spinner';
import { Pagination } from '@/ui/Pagination';
import {
  FederationCompetitorMembersDocument,
  FederationCompetitorHistoryDocument,
} from '@/graphql/Federation';
import { formatDateOnly, formatResultStatus } from '@/ui/format';

export function Members({ federation, id }: { federation: string; id: string }) {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: FederationCompetitorMembersDocument,
    variables: (offset) => ({
      id,
      membersOffset: offset,
    }),
    limit: 100,
    pageKey: 'membersPage',
    getTotal: (data) => data?.federationCompetitor?.components?.totalCount,
  });
  const members = data?.federationCompetitor?.components;
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
      <Table columns={['person', 'role']} empty="No members">
        {members.nodes.map(({ personId, person, role }) => (
          <tr key={personId} className="border-t border-neutral-4">
            <th scope="row">
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
            <td>{role.toLowerCase()}</td>
          </tr>
        ))}
      </Table>
      <Pagination {...pagination} />
    </>
  );
}

export function History({ federation, id }: { federation: string; id: string }) {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: FederationCompetitorHistoryDocument,
    variables: (offset) => ({
      id,
      historyOffset: offset,
    }),
    limit: 50,
    getTotal: (data) => data?.competitions?.totalCount,
  });
  const history = data?.competitions;
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
      <Table
        columns={[
          'date',
          'event',
          'competition',
          'status',
          'registration',
          'rank',
          showPoints && 'points',
          showFinalGain && 'final gain',
        ]}
        empty="No registrations or results"
      >
        {history.nodes.map((row) => {
          const entry = row.entries.nodes[0];
          const result = row.results.nodes[0];
          const status = formatResultStatus(result?.completionStatus);
          return (
            <tr key={row.id} className="border-t border-neutral-4">
              <td className="whitespace-nowrap">{formatDateOnly(row.startDate)}</td>
              <td>
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
              <th scope="row">
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
              <td className="whitespace-nowrap">
                {row.status.toLowerCase().replaceAll('_', ' ')}
              </td>
              <td>
                {entry?.cancelled ? 'cancelled' : !result && entry ? 'registered' : ''}
              </td>
              <td className="whitespace-nowrap">
                {result?.ranking}
                {result?.rankingTo && result?.rankingTo !== result?.ranking
                  ? `–${result?.rankingTo}`
                  : ''}
              </td>
              {showPoints && (
                <td>{result?.pointGain == null ? null : Number(result.pointGain)}</td>
              )}
              {showFinalGain && (
                <td>
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
      </Table>
      <Pagination {...pagination} />
    </>
  );
}
