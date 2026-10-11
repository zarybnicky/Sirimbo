import Link from 'next/link';
import { notFound } from 'next/navigation';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { Pagination } from '@/ui/Pagination';
import { numericDateWithYearFormatter } from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { runQuery } from '@/lib/server/postgresql';
import {
  browseCompetitor,
  browseCompetitorMembers,
  browseCompetitorHistoryCount,
  browseCompetitorHistory,
} from '../../../federated.queries';

export const metadata = {
  title: 'Federated competitor',
  robots: { index: false, follow: false },
};

export default async function CompetitorPage({
  params,
  searchParams,
}: {
  params: Promise<{ federation: string; id: string }>;
  searchParams: Promise<{ page?: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;
  const { federation, id: externalId } = await params;
  if (!/^[0-9]{1,18}$/.test(externalId)) notFound();
  const id = `${federation}:${externalId}`;
  const { page } = await searchParams;
  const [competitor] = await runQuery(browseCompetitor, { federation, id });
  if (!competitor) notFound();
  const [members, [count]] = await Promise.all([
    runQuery(browseCompetitorMembers, { id }),
    runQuery(browseCompetitorHistoryCount, { id }),
  ]);
  const total = count?.total ?? 0;
  const currentPage = Math.min(
    Math.max(1, Number.parseInt(page ?? '1', 10) || 1),
    Math.max(1, Math.ceil(total / 50)),
  );
  const history = await runQuery(browseCompetitorHistory, {
    id,
    offset: (currentPage - 1) * 50,
  });
  const title = competitor.name || competitor.id;
  const base = `/admin/federated/${federation}`;
  return (
    <Layout requireSystemAdmin>
      <PageHeader
        title={title}
        breadcrumbs={[
          { label: 'Federations', href: '/admin/federated' },
          { label: competitor.federation_name, href: `${base}?view=competitors` },
          { label: title },
        ]}
      />
      <dl className="text-sm tabular">
        <dt>competitor ID</dt>
        <dd>{competitor.id}</dd>
        <dt>type</dt>
        <dd>{competitor.competitor_type}</dd>
        <dt>age group</dt>
        <dd>{competitor.age_group || '-'}</dd>
      </dl>
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
            {members.map((member) => (
              <tr key={member.id} className="border-t border-neutral-4">
                <th scope="row" className="px-2 py-1 font-normal">
                  <Link
                    className="text-accent-11 underline"
                    href={`${base}/people/${member.external_id}`}
                  >
                    {member.name}
                  </Link>
                </th>
                <td className="px-2 py-1">{member.id}</td>
                <td className="px-2 py-1">{member.role}</td>
              </tr>
            ))}
            {members.length === 0 && (
              <tr>
                <td colSpan={3} className="px-2 py-1">
                  No members
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <h2 className="mt-4 mb-2 text-lg font-bold">Registrations and results</h2>
      <div className="col-full-width overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {[
                'date',
                'event',
                'competition',
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
            {history.map((row) => (
              <tr key={row.id} className="border-t border-neutral-4">
                <td className="px-2 py-1 whitespace-nowrap">
                  {numericDateWithYearFormatter.format(row.start_date)}
                </td>
                <td className="px-2 py-1">
                  <Link
                    className="text-accent-11 underline"
                    href={`${base}/${row.event_id}`}
                    prefetch={false}
                  >
                    {row.event_name || row.event_external_id}
                  </Link>
                </td>
                <th scope="row" className="px-2 py-1 font-normal">
                  <Link
                    className="text-accent-11 underline"
                    href={`${base}/${row.event_id}/${row.id}`}
                    prefetch={false}
                  >
                    {row.category}
                  </Link>
                </th>
                <td className="px-2 py-1">
                  {row.cancelled === null
                    ? '-'
                    : row.cancelled
                      ? 'cancelled'
                      : 'registered'}
                </td>
                <td className="px-2 py-1 whitespace-nowrap">
                  {row.ranking}
                  {row.ranking_to && row.ranking_to !== row.ranking
                    ? `–${row.ranking_to}`
                    : ''}
                </td>
                <td className="px-2 py-1">{row.point_gain}</td>
                <td className="px-2 py-1">{row.final_gain}</td>
              </tr>
            ))}
            {history.length === 0 && (
              <tr>
                <td colSpan={7} className="px-2 py-1">
                  No registrations or results
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <Pagination
        total={total}
        limit={50}
        page={currentPage}
        href={(page) => `${base}/competitors/${externalId}?page=${page}`}
      />
    </Layout>
  );
}
