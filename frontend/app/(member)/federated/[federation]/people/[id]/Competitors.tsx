'use client';

import { Table } from '@/ui/Table';
import Link from 'next/link';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { FormError } from '@/ui/form';
import { Spinner } from '@/ui/Spinner';
import { Pagination } from '@/ui/Pagination';
import { FederationPersonCompetitorsDocument } from '@/graphql/Federation';

export function Competitors({
  federation,
  externalId,
}: {
  federation: string;
  externalId: string;
}) {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: FederationPersonCompetitorsDocument,
    variables: (offset) => ({ federation, externalId, competitorsOffset: offset }),
    limit: 100,
    getTotal: (data) =>
      data?.federationPersonByFederationAndExternalId?.competitorComponents?.totalCount,
  });
  if (error) return <FormError error={error} />;
  if (!data || fetching) return <Spinner />;

  const competitors =
    data.federationPersonByFederationAndExternalId?.competitorComponents.nodes || [];

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Soutěžící</h2>
      <Table columns={['Soutěžící', 'Typ', 'Role']} empty="Žádní soutěžící">
        {competitors.map(({ competitorId, competitor, role }) => (
          <tr key={competitorId} className="border-t border-neutral-4">
            <th scope="row">
              {competitor ? (
                <Link
                  className="text-accent-11 underline"
                  href={`/federated/${federation}/competitors/${competitor.externalId}`}
                >
                  {competitor.name || competitorId}
                </Link>
              ) : (
                competitorId
              )}
            </th>
            <td>{competitor?.competitorType.toLowerCase()}</td>
            <td>{role.toLowerCase()}</td>
          </tr>
        ))}
      </Table>
      <Pagination {...pagination} />
    </>
  );
}
