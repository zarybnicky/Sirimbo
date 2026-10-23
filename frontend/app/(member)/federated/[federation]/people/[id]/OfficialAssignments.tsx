'use client';

import { formatCategoryName } from '@/ui/csts';

import { Table } from '@/ui/Table';
import Link from 'next/link';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { FederationPersonOfficialAssignmentsDocument } from '@/graphql/Federation';
import { cn } from '@/lib/cn';
import { FormError } from '@/ui/form';
import { formatCount, formatDateOnly } from '@/ui/format';
import { Pagination } from '@/ui/Pagination';
import { Spinner } from '@/ui/Spinner';

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
    getTotal: (data) => data?.federationEvents?.totalCount,
  });

  if (error) return <FormError error={error} />;
  if (!data || fetching) return <Spinner />;
  const events = data.federationEvents?.nodes || [];

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Official assignments</h2>

      <Table columns={['Datum', 'Soutěž', 'Role']}>
        {events.map((event) => {
          const roles = [
            ...new Set([
              ...event.officials.nodes.map((x) => x.role),
              ...event.competitionsList.flatMap((comp) =>
                comp.officials.nodes.map((x) => x.role),
              ),
            ]),
          ].toSorted();
          return (
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
                {roles.map((role) => {
                  const competitions = event.competitionsList.filter((comp) =>
                    comp.officials.nodes.some((x) => x.role === role),
                  );
                  return (
                    <div key={role}>
                      {role.toLowerCase().replaceAll('_', ' ')}
                      {event.officials.nodes
                        .filter((x) => x.role === role)
                        .map(({ discipline, grade }) => (
                          <span key={discipline} className="text-neutral-11">
                            {[discipline, grade]
                              .filter(Boolean)
                              .map((x) => ` · ${x}`)
                              .join('')}
                          </span>
                        ))}
                      {competitions.length > 0 && (
                        <details>
                          <summary className="cursor-pointer text-neutral-11">
                            {formatCount(competitions.length, {
                              one: 'soutěž',
                              few: 'soutěže',
                              other: 'soutěží',
                            })}
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
                                  {formatCategoryName(
                                    federation,
                                    comp.category,
                                    comp.competitionType,
                                  ) || comp.externalId}
                                </Link>
                                {comp.startDate !== event.startDate && comp.startDate && (
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
          );
        })}
      </Table>
      <Pagination {...pagination} />
    </>
  );
}
