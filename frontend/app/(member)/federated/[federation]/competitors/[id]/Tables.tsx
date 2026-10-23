'use client';

import { formatCategoryName } from '@/ui/csts';

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
    variables: (offset) => ({ id, membersOffset: offset }),
    limit: 100,
    pageKey: 'membersPage',
    getTotal: (data) => data?.federationCompetitor?.components?.totalCount,
  });

  if (error) return <FormError error={error} />;
  if (fetching) return <Spinner />;
  const members = data?.federationCompetitor?.components.nodes || [];

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Osoby</h2>

      <Table columns={['person', 'role']} empty="Žádné osoby">
        {members.map(({ personId, person, role }) => (
          <tr key={personId} className="border-t border-neutral-4">
            <th scope="row">
              {person ? (
                <Link
                  className="text-accent-11 underline"
                  href={`/federated/${federation}/people/${person.externalId}`}
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
  if (error) return <FormError error={error} />;
  if (fetching || !data) return <Spinner />;
  const history = data.competitions?.nodes || [];

  const showPoints = history.some((row) =>
    row.results.nodes.some((x) => x.pointGain != null),
  );

  return (
    <>
      {fetching && (
        <p role="status" className="my-2 text-sm text-neutral-11">
          Loading…
        </p>
      )}
      <h2 className="mt-4 mb-2 text-lg font-bold">Soutěže</h2>
      <Table
        columns={[
          'Datum',
          'Soutěž',
          'Kategorie',
          'Umístění',
          showPoints && 'Body',
        ]}
        empty="No registrations or results"
      >
        {history.map((row) => {
          const categoryName =
            formatCategoryName(federation, row.category, row.competitionType) ||
            row.externalId;
          const result = row.results.nodes[0];
          const cancelled = row.entries.nodes[0]?.cancelled;
          const status = formatResultStatus(result?.completionStatus);
          return (
            <tr key={row.id} className="border-t border-neutral-4">
              <td className="whitespace-nowrap">{formatDateOnly(row.startDate)}</td>
              <td>
                {row.event && (
                  <Link
                    className="text-accent-11 underline"
                    href={`/federated/${federation}/${row.event.externalId}`}
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
                      cancelled || row.status === 'CANCELLED' ? 'line-through' : 'underline',
                    )}
                    title={cancelled ? 'Odhlášená registrace' : undefined}
                    href={`/federated/${federation}/${row.event.externalId}/${row.externalId}`}
                    prefetch={false}
                  >
                    {categoryName}
                  </Link>
                ) : (
                  <span className={cancelled ? 'line-through' : undefined}>
                    {categoryName}
                  </span>
                )}
                {status && <div className="text-xs text-neutral-11">{status}</div>}
              </th>
              <td className="whitespace-nowrap">
                {result?.ranking}
                {result?.rankingTo && result?.rankingTo !== result?.ranking
                  ? `–${result?.rankingTo}`
                  : ''}
              </td>
              {showPoints && (
                <td>
                  {result?.pointGain == null ? null : Number(result.pointGain)}
                  {federation !== 'wdsf'
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
