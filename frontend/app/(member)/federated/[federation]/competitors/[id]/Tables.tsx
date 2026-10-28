'use client';

import { formatCategoryName } from '@/ui/csts';

import { Table } from '@/ui/Table';
import Link from 'next/link';
import { cn } from '@/lib/cn';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Pagination } from '@/ui/Pagination';
import { FederationCompetitorMembersDocument } from '@/graphql/Federation';
import {
  CompetitionsDocument,
  type CompetitionsQueryVariables,
} from '@/graphql/CompetitionParticipation';
import { formatDateOnly, formatPointsAndFinals, formatResultStatus } from '@/ui/format';

export function Members({ federation, id }: { federation: string; id: string }) {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: FederationCompetitorMembersDocument,
    variables: (offset) => ({ id, membersOffset: offset }),
    limit: 100,
    pageKey: 'membersPage',
    getTotal: (data) => data?.federationCompetitor?.components?.totalCount,
  });

  const members = data?.federationCompetitor?.components.nodes || [];

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Osoby</h2>

      <Table
        columns={['person', 'role']}
        busy={fetching}
        error={error}
        empty="Žádné osoby"
      >
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
    query: CompetitionsDocument,
    variables: (offset): CompetitionsQueryVariables => ({
      condition: { competitorId: id },
      entryCondition: { competitorId: id },
      resultCondition: { competitorId: id },
      offset,
      orderBy: ['START_DATE_DESC', 'ID_DESC'],
    }),
    limit: 50,
    getTotal: (data) => data?.competitions?.totalCount,
  });
  const history = data?.competitions?.nodes || [];

  const showPoints = history.some((row) =>
    row.results?.some((x) => x.pointGain != null),
  );

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Soutěže</h2>
      <Table
        busy={fetching}
        error={error}
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
          const result = row.results?.[0];
          const cancelled = row.entries?.[0]?.cancelled;
          const status = formatResultStatus(result?.completionStatus);
          return (
            <tr key={row.id} className="border-t border-neutral-4">
              <td className="whitespace-nowrap">
                {row.startDate && formatDateOnly(row.startDate)}
              </td>
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
                    title={cancelled ? 'Zrušená přihláška' : undefined}
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
                {!!row.participantsTotal && `/${row.participantsTotal}`}
              </td>
              {showPoints && (
                <td className="whitespace-nowrap">
                  {formatPointsAndFinals(
                    result?.pointGain,
                    result?.finalGain,
                    'gain',
                  )}
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
