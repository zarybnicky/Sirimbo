'use client';

import Link from 'next/link';
import { useEffect } from 'react';
import { parseAsInteger, useQueryState } from 'nuqs';
import { useQuery } from 'urql';
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
  const [currentPage, setPage] = useQueryState(
    'page',
    parseAsInteger
      .withDefault(1)
      .withOptions({ shallow: true, scroll: false, history: 'push' }),
  );
  const [{ data, fetching, error }] = useQuery({
    query: FederationPersonCompetitorsDocument,
    variables: {
      federation,
      externalId,
      competitorsOffset: Math.min(Math.max(0, currentPage - 1) * 100, 2_147_483_647),
    },
  });
  const competitors =
    data?.federationPersonByFederationAndExternalId?.competitorComponents;
  const total = competitors?.totalCount;
  useEffect(() => {
    if (fetching || error || total == null) return;
    const page = Math.max(1, Math.min(currentPage, Math.ceil(total / 100)));
    if (page !== currentPage) void setPage(page, { history: 'replace' });
  }, [currentPage, total, fetching, error, setPage]);
  const base = `/federated/${federation}`;
  if (error) return <FormError error={error} />;
  if (!competitors) return <Spinner />;

  return (
    <>
      {fetching && (
        <p role="status" className="my-2 text-sm text-neutral-11">
          Loading…
        </p>
      )}
      <h2 className="mt-4 mb-2 text-lg font-bold">Competitors</h2>
      <div className="overflow-x-auto">
        <table className="w-full text-left text-sm">
          <thead>
            <tr>
              {['competitor', 'type', 'role'].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {competitors.nodes.map(({ competitorId, competitor, role }) => (
              <tr key={competitorId} className="border-t border-neutral-4">
                <th scope="row" className="px-2 py-1 font-normal">
                  {competitor ? (
                    <Link
                      className="text-accent-11 underline"
                      href={`${base}/competitors/${competitor.externalId}`}
                    >
                      {competitor.name || competitorId}
                    </Link>
                  ) : (
                    competitorId
                  )}
                </th>
                <td className="px-2 py-1">{competitor?.competitorType.toLowerCase()}</td>
                <td className="px-2 py-1">{role.toLowerCase()}</td>
              </tr>
            ))}
            {competitors.nodes.length === 0 && (
              <tr>
                <td colSpan={3} className="px-2 py-1">
                  No competitors
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <Pagination
        total={competitors.totalCount}
        limit={100}
        page={currentPage}
        onPageChange={setPage}
      />
    </>
  );
}
