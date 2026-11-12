'use client';

import { formatCategoryName } from '@/ui/csts';

import { Table } from '@/ui/Table';
import Link from 'next/link';
import { cn } from '@/lib/cn';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Pagination } from '@/ui/Pagination';
import {
  CompetitionsDocument,
  type CompetitionsQueryVariables,
} from '@/graphql/CompetitionParticipation';
import { formatDateOnly, formatPointsAndFinals, formatResultStatus } from '@/ui/format';

export function History({ federation, id }: { federation: string; id: string }) {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: CompetitionsDocument,
    variables: (offset): CompetitionsQueryVariables => ({
      competitorId: id,
      offset,
      orderBy: ['START_DATE_DESC', 'ID_DESC'],
    }),
    limit: 50,
    getTotal: (data) => data.competitions?.totalCount,
  });
  const history = data?.competitions?.nodes || [];

  const showPoints = history.some((row) => row.participations.some((x) => x.pointGain != null));

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Soutěže</h2>
      <Table
        busy={fetching}
        error={error}
        columns={['Datum', 'Soutěž', 'Kategorie', 'Umístění', showPoints && 'Body']}
        empty="No registrations or results"
      >
        {history.map((row) => {
          const categoryName = formatCategoryName(row) || row.externalId;
          const result = row.participations[0];
          const cancelled = result?.cancelled;
          return (
            <tr key={row.id}>
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
                      cancelled || row.status === 'CANCELLED'
                        ? 'line-through'
                        : 'underline',
                    )}
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
                <div className="text-xs text-neutral-11">
                  {formatResultStatus(result?.completionStatus)}
                </div>
              </th>
              <td className="whitespace-nowrap">
                {result?.ranking && (
                  <>
                    {result.ranking}
                    {result.rankingTo && result.rankingTo !== result.ranking
                      ? `–${result.rankingTo}.`
                      : '.'}
                    {!!row.participantsTotal && `/${row.participantsTotal}`}
                  </>
                )}
              </td>
              {showPoints && (
                <td className="whitespace-nowrap">
                  {formatPointsAndFinals(result, 'gain')}
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
