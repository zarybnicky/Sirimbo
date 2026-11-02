'use client';

import { FederationPersonOfficialAssignmentsDocument } from '@/graphql/Federation';
import { cn } from '@/lib/cn';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { formatCategoryName } from '@/ui/csts';
import { formatDateOnly, formatOfficialRole } from '@/ui/format';
import { Pagination } from '@/ui/Pagination';
import { Table } from '@/ui/Table';
import Link from 'next/link';

export function OfficialAssignments({
  federation,
  id,
}: {
  federation: string;
  id: string;
}) {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: FederationPersonOfficialAssignmentsDocument,
    variables: (offset) => ({ id, offset }),
    limit: 20,
    pageKey: 'officialsPage',
    getTotal: (data) => data.federationEvents?.totalCount,
  });

  const events = data?.federationEvents?.nodes || [];
  if (events.length === 0) return null;

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Official assignments</h2>

      <Table columns={['Datum', 'Soutěž', 'Role']} busy={fetching} error={error}>
        {events.map((event) => (
          <tr key={event.id} className="border-t border-neutral-4 align-top">
            <td className="whitespace-nowrap">{formatDateOnly(event.startDate)}</td>
            <th scope="row">
              <Link
                href={`/federated/${federation}/${event.externalId}`}
                prefetch={false}
                className={cn(
                  'text-accent-11',
                  event.status === 'CANCELLED' ? 'line-through' : 'underline',
                )}
              >
                {event.name || event.externalId}
              </Link>
            </th>
            <td className="space-y-2">
              {[
                ...new Set([
                  ...event.officials.nodes.map((x) => x.role),
                  ...event.competitionsList.flatMap((comp) =>
                    comp.officials.nodes.map((x) => x.role),
                  ),
                ]),
              ]
                .toSorted()
                .map((role) => {
                  const competitions = event.competitionsList.filter((comp) =>
                    comp.officials.nodes.some((x) => x.role === role),
                  );
                  return (
                    <div key={role}>
                      {competitions.length === 0 ? (
                        formatOfficialRole(role)
                      ) : (
                        <details>
                          <summary className="cursor-pointer">
                            {formatOfficialRole(role)} ({competitions.length})
                          </summary>
                          <ul className="space-y-1 mt-1">
                            {competitions.map((comp) => (
                              <li key={comp.id}>
                                <Link
                                  href={`/federated/${federation}/${event.externalId}/${comp.externalId}`}
                                  prefetch={false}
                                  className={cn(
                                    'text-accent-11',
                                    comp.status === 'CANCELLED'
                                      ? 'line-through'
                                      : 'underline',
                                  )}
                                >
                                  {formatCategoryName(comp)}
                                </Link>
                                {comp.startDate !== event.startDate && (
                                  <span className="text-neutral-11">
                                    {' '}
                                    · {formatDateOnly(comp.startDate)}
                                  </span>
                                )}
                              </li>
                            ))}
                          </ul>
                        </details>
                      )}
                    </div>
                  );
                })}
            </td>
          </tr>
        ))}
      </Table>
      <Pagination {...pagination} />
    </>
  );
}
