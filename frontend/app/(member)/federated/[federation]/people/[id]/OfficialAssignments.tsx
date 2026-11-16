'use client';

import { FederationPersonOfficialAssignmentsDocument } from '@/graphql/Federation';
import { cn } from '@/lib/cn';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { formatCategoryName } from '@/ui/csts';
import { formatDateOnly, formatOfficialRole } from '@/ui/format';
import { Pagination } from '@/ui/Pagination';
import { Table } from '@/ui/Table';
import Link from 'next/link';
import { Fragment, useState } from 'react';
import { ChevronRight } from 'lucide-react';

export function OfficialAssignments({
  federation,
  id,
}: {
  federation: string;
  id: string;
}) {
  const [expanded, setExpanded] = useState<Set<string>>(() => new Set());
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
            <Fragment key={event.id}>
              <tr className="align-top">
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
                <td>
                  {event.competitionsList.some(
                    (comp) => comp.officials.nodes.length > 0,
                  ) ? (
                    <button
                      type="button"
                      aria-expanded={expanded.has(event.id)}
                      className="inline-flex cursor-pointer items-center gap-1 text-left text-accent-11 [&[aria-expanded=true]>svg]:rotate-90"
                      onClick={() =>
                        setExpanded((previous) => {
                          const next = new Set(previous);
                          if (next.has(event.id)) next.delete(event.id);
                          else next.add(event.id);
                          return next;
                        })
                      }
                    >
                      <ChevronRight aria-hidden className="size-4 shrink-0" />
                      {roles.map(formatOfficialRole).join(', ')}
                    </button>
                  ) : (
                    roles.map(formatOfficialRole).join(', ')
                  )}
                </td>
              </tr>
              {expanded.has(event.id) &&
                event.competitionsList.some(
                  (comp) => comp.officials.nodes.length > 0,
                ) && (
                  <tr>
                    <td colSpan={3}>
                      <div className="space-y-2">
                        {roles.map((role) => {
                          const competitions = event.competitionsList.filter((comp) =>
                            comp.officials.nodes.some((x) => x.role === role),
                          );
                          if (competitions.length === 0) return null;
                          return (
                            <div key={role}>
                              <div className="font-medium">
                                {formatOfficialRole(role)} ({competitions.length})
                              </div>
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
                            </div>
                          );
                        })}
                      </div>
                    </td>
                  </tr>
                )}
            </Fragment>
          );
        })}
      </Table>
      <Pagination {...pagination} />
    </>
  );
}
