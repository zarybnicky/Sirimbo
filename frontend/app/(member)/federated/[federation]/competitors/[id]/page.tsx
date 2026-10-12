import Link from 'next/link';
import { notFound } from 'next/navigation';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { Pagination } from '@/ui/Pagination';
import {
  formatCoupleName,
  formatOpenDateRange,
  numericDateWithYearFormatter,
} from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationCompetitorDocument } from '@/graphql/Federation';

export const metadata = {
  title: 'Federated competitor',
  robots: { index: false, follow: false },
};

export default async function CompetitorPage({
  params,
  searchParams,
}: {
  params: Promise<{ federation: string; id: string }>;
  searchParams: Promise<{ page?: string; membersPage?: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federation, id: externalId } = await params;
  if (!/^[0-9]{1,18}$/.test(externalId)) notFound();
  const id = `${federation}:${externalId}`;
  const { page, membersPage } = await searchParams;
  const currentPage = Math.max(1, Number.parseInt(page ?? '1', 10) || 1);
  const currentMembersPage = Math.max(1, Number.parseInt(membersPage ?? '1', 10) || 1);
  const { federationCompetitor: competitor, competitions: history } =
    await executeGraphql(FederationCompetitorDocument, {
      id,
      historyOffset: Math.min((currentPage - 1) * 50, 2_147_483_647),
      membersOffset: Math.min((currentMembersPage - 1) * 100, 2_147_483_647),
    });
  if (!competitor || !history) notFound();
  const members = competitor.components;
  if (currentMembersPage > Math.max(1, Math.ceil(members.totalCount / 100))) notFound();
  if (currentPage > Math.max(1, Math.ceil(history.totalCount / 50))) notFound();
  const title = competitor.name || competitor.id;
  const base = `/federated/${federation}`;
  return (
    <Layout requireUser>
      <PageHeader
        title={title}
        breadcrumbs={[
          { label: 'Federations', href: '/federated' },
          {
            label: competitor.federationByFederation?.name ?? federation,
            href: `${base}?view=competitors`,
          },
          { label: title },
        ]}
      />
      <dl className="text-sm tabular">
        <dt>competitor ID</dt>
        <dd>{competitor.id}</dd>
        <dt>type</dt>
        <dd>{competitor.competitorType.toLowerCase()}</dd>
        <dt>age group</dt>
        <dd>{competitor.ageGroup || '-'}</dd>
      </dl>
      <h2 className="mt-4 mb-2 text-lg font-bold">Local matches</h2>
      <div className="overflow-x-auto">
        <table className="w-full text-left text-sm">
          <thead>
            <tr>
              {['couple', 'partnership'].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {competitor.couplesList.map((couple) => (
              <tr key={couple.id} className="border-t border-neutral-4">
                <th scope="row" className="px-2 py-1 font-normal">
                  <Link className="text-accent-11 underline" href={`/pary/${couple.id}`}>
                    {formatCoupleName(couple)}
                  </Link>
                </th>
                <td className="px-2 py-1">{formatOpenDateRange(couple)}</td>
              </tr>
            ))}
            {competitor.couplesList.length === 0 && (
              <tr>
                <td colSpan={2} className="px-2 py-1">
                  No local matches
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <h2 className="mt-4 mb-2 text-lg font-bold">Members</h2>
      <div className="overflow-x-auto">
        <table className="w-full text-left text-sm">
          <thead>
            <tr>
              {['person', 'person ID', 'role'].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {members.nodes.map(({ personId, person, role }) => (
              <tr key={personId} className="border-t border-neutral-4">
                <th scope="row" className="px-2 py-1 font-normal">
                  {person ? (
                    <Link
                      className="text-accent-11 underline"
                      href={`${base}/people/${person.externalId}`}
                    >
                      {person.canonicalName ||
                        [person.firstName, person.lastName].filter(Boolean).join(' ') ||
                        personId}
                    </Link>
                  ) : (
                    personId
                  )}
                </th>
                <td className="px-2 py-1">{personId}</td>
                <td className="px-2 py-1">{role.toLowerCase()}</td>
              </tr>
            ))}
            {members.nodes.length === 0 && (
              <tr>
                <td colSpan={3} className="px-2 py-1">
                  No members
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <Pagination
        total={members.totalCount}
        limit={100}
        page={currentMembersPage}
        href={(membersPage) =>
          `${base}/competitors/${externalId}?page=${currentPage}&membersPage=${membersPage}`
        }
      />
      <h2 className="mt-4 mb-2 text-lg font-bold">Registrations and results</h2>
      <div className="col-full-width overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {[
                'date',
                'event',
                'competition',
                'status',
                'registration',
                'rank',
                'points',
                'final gain',
              ].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {history.nodes.map((row) => {
              const entry = row.entries.nodes[0];
              const result = row.results.nodes[0];
              return (
                <tr key={row.id} className="border-t border-neutral-4">
                  <td className="px-2 py-1 whitespace-nowrap">
                    {numericDateWithYearFormatter.format(new Date(row.startDate))}
                  </td>
                  <td className="px-2 py-1">
                    <Link
                      className="text-accent-11 underline"
                      href={`${base}/${row.eventId}`}
                      prefetch={false}
                    >
                      {row.event?.name || row.event?.externalId || row.eventId}
                    </Link>
                  </td>
                  <th scope="row" className="px-2 py-1 font-normal">
                    <Link
                      className="text-accent-11 underline"
                      href={`${base}/${row.eventId}/${row.id}`}
                      prefetch={false}
                    >
                      {row.category?.name || row.externalId}
                    </Link>
                  </th>
                  <td className="px-2 py-1 whitespace-nowrap">
                    {row.status.toLowerCase().replaceAll('_', ' ')}
                  </td>
                  <td className="px-2 py-1">
                    {!entry ? '-' : entry.cancelled ? 'cancelled' : 'registered'}
                  </td>
                  <td className="px-2 py-1 whitespace-nowrap">
                    {result?.ranking}
                    {result?.rankingTo && result?.rankingTo !== result?.ranking
                      ? `–${result?.rankingTo}`
                      : ''}
                  </td>
                  <td className="px-2 py-1">{result?.pointGain}</td>
                  <td className="px-2 py-1">{result?.finalGain}</td>
                </tr>
              );
            })}
            {history.nodes.length === 0 && (
              <tr>
                <td colSpan={8} className="px-2 py-1">
                  No registrations or results
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <Pagination
        total={history.totalCount}
        limit={50}
        page={currentPage}
        href={(page) =>
          `${base}/competitors/${externalId}?page=${page}&membersPage=${currentMembersPage}`
        }
      />
    </Layout>
  );
}
