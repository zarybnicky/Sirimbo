import Link from 'next/link';
import { notFound } from 'next/navigation';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { Members, History } from './Tables';
import { formatCoupleName, formatOpenDateRange } from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationCompetitorDocument } from '@/graphql/Federation';

export const metadata = {
  title: 'Federated competitor',
  robots: { index: false, follow: false },
};

export default async function CompetitorPage({
  params,
}: {
  params: Promise<{ federation: string; id: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federation, id: externalId } = await params;
  if (!/^[0-9]{1,18}$/.test(externalId)) notFound();
  const id = `${federation}:${externalId}`;
  const { federationCompetitor: competitor } = await executeGraphql(
    FederationCompetitorDocument,
    { id },
  );
  if (!competitor) notFound();
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
      <h2 className="mt-4 mb-2 text-lg font-bold">Category progress</h2>
      <div className="overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {['category', 'points'].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {competitor.categoryProgresses.nodes
              .toSorted((a, b) =>
                (a.category?.name ?? a.categoryId).localeCompare(
                  b.category?.name ?? b.categoryId,
                ),
              )
              .map((progress) => (
                <tr key={progress.categoryId} className="border-t border-neutral-4">
                  <th scope="row" className="px-2 py-1 font-normal">
                    {progress.category?.name || progress.categoryId}
                  </th>
                  <td className="px-2 py-1">
                    {Number(progress.points)}F
                    {progress.domesticFinals + progress.foreignFinals}
                  </td>
                </tr>
              ))}
            {competitor.categoryProgresses.nodes.length === 0 && (
              <tr>
                <td colSpan={2} className="px-2 py-1">
                  No category progress
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
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
      <Members federation={federation} id={id} />
      <History federation={federation} id={id} />
    </Layout>
  );
}
