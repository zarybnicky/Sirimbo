import { Table } from '@/ui/Table';
import Link from 'next/link';
import { notFound } from 'next/navigation';
import type { Metadata } from 'next';
import { cache } from 'react';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { Members, History } from './Tables';
import { formatCoupleName, formatOpenDateRange } from '@/ui/format';
import { formatAgeGroup } from '@/ui/csts';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationCompetitorDocument } from '@/graphql/Federation';

type Props = {
  params: Promise<{ federation: string; id: string }>;
};

const getCompetitor = cache(async (federation: string, externalId: string) => {
  if (!/^[0-9]{1,18}$/.test(externalId)) notFound();
  const id = `${federation}:${externalId}`;
  const { federationCompetitor: competitor } = await executeGraphql(
    FederationCompetitorDocument,
    { id },
  );
  if (!competitor) notFound();
  return competitor;
});

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return { robots: { index: false, follow: false } };
  const { federation, id } = await params;
  const competitor = await getCompetitor(federation, id);
  return {
    title: `${competitor.name || competitor.id} · ${competitor.federationByFederation?.name ?? federation}`,
    robots: { index: false, follow: false },
  };
}

export default async function CompetitorPage({ params }: Props) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federation, id: externalId } = await params;
  const competitor = await getCompetitor(federation, externalId);
  const id = competitor.id;
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
        <dt>type</dt>
        <dd>{competitor.competitorType.toLowerCase()}</dd>
        <dt>age group</dt>
        <dd>{formatAgeGroup(competitor.ageGroup) || '-'}</dd>
      </dl>
      <h2 className="mt-4 mb-2 text-lg font-bold">Category progress</h2>
      <Table columns={['category', 'points']} empty="No category progress">
        {competitor.categoryProgressesList
          .toSorted((a, b) =>
            (a.category?.name ?? a.categoryId).localeCompare(
              b.category?.name ?? b.categoryId,
            ),
          )
          .map((progress) => (
            <tr key={progress.categoryId} className="border-t border-neutral-4">
              <th scope="row">{progress.category?.name || progress.categoryId}</th>
              <td>
                {Number(progress.points)}F
                {progress.domesticFinals + progress.foreignFinals}
              </td>
            </tr>
          ))}
      </Table>
      {competitor.couplesList.length > 0 && (
        <>
          <h2 className="mt-4 mb-2 text-lg font-bold">Local matches</h2>
          <Table columns={['couple', 'partnership']}>
            {competitor.couplesList.map((couple) => (
              <tr key={couple.id} className="border-t border-neutral-4">
                <th scope="row">
                  <Link className="text-accent-11 underline" href={`/pary/${couple.id}`}>
                    {formatCoupleName(couple)}
                  </Link>
                </th>
                <td>{formatOpenDateRange(couple)}</td>
              </tr>
            ))}
          </Table>
        </>
      )}
      <Members federation={federation} id={id} />
      <History federation={federation} id={id} />
    </Layout>
  );
}
