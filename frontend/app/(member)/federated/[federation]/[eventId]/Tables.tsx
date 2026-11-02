'use client';

import { formatCategoryName } from '@/ui/csts';

import { Table } from '@/ui/Table';
import Link from 'next/link';
import { cn } from '@/lib/cn';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Pagination } from '@/ui/Pagination';
import { FederationEventCompetitionsDocument } from '@/graphql/Federation';
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
  const competitions =
    data?.federationEventByFederationAndExternalId?.competitions.nodes ?? [];
  const showDate = competitions.some(
    (comp) => comp.startDate !== competitions[0]?.startDate,
  );

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Soutěže</h2>
      <Table
        busy={fetching}
        error={error}
        columns={[
          'Kategorie',
          showDate && 'Datum',
          'Prezence',
          <span key="participants" className="block text-right">
            Účastníci
          </span>,
        ]}
      >
        {competitions.map((comp) => (
          <tr key={comp.id} className="border-t border-neutral-4">
            <th scope="row">
              <Link
                className={cn(
                  'text-accent-11',
                  comp.status === 'CANCELLED' ? 'line-through' : 'underline',
                )}
                href={`/federated/${federation}/${eventId}/${comp.externalId}`}
                prefetch={false}
              >
                {formatCategoryName(comp) || comp.externalId}
              </Link>
            </th>
            {showDate && (
              <td className="whitespace-nowrap">{formatDateOnly(comp.startDate)}</td>
            )}
            <td>{comp.checkInEnd?.slice(0, 5)}</td>
            <td className="text-right">
              {comp.results.totalCount || comp.entries.totalCount}
            </td>
          </tr>
        ))}
      </Table>
      <Pagination {...pagination} />
    </>
  );
}
