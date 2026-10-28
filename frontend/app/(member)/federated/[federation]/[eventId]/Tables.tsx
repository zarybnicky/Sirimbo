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
  const competitions = data?.federationEventByFederationAndExternalId?.competitions;
  const path = `/federated/${federation}/${eventId}`;

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Soutěže</h2>
      <Table
        busy={fetching}
        error={error}
        columns={[
          'Kategorie',
          'Typ',
          'Stav',
          'Datum',
          'Konec prezence',
          'Účastníci',
        ]}
        empty="No competitions"
      >
        {competitions?.nodes.map((competition) => (
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
                {formatCategoryName(
                  federation,
                  competition.category,
                  competition.competitionType,
                ) || competition.externalId}
              </Link>
            </th>
            <td>{competition.competitionType?.toLowerCase().replaceAll('_', ' ')}</td>
            <td className="whitespace-nowrap">
              {competition.status.toLowerCase().replaceAll('_', ' ')}
            </td>
            <td className="whitespace-nowrap">{formatDateOnly(competition.startDate)}</td>
            <td>{competition.checkInEnd?.slice(0, 5)}</td>
            <td>{competition.results.totalCount || competition.entries.totalCount}</td>
          </tr>
        ))}
      </Table>
      <Pagination {...pagination} />
    </>
  );
}
