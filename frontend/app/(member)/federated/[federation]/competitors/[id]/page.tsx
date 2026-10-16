import { DefinitionList } from '@/ui/DefinitionList';
import { FederationCategoryProgress } from '@/ui/FederationCategoryProgress';
import { Table } from '@/ui/Table';
import Link from 'next/link';
import { notFound } from 'next/navigation';
import type { Metadata } from 'next';
import { cache } from 'react';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/PageHeader';
import { Members, History } from './Tables';
import { formatCoupleName, formatOpenDateRange } from '@/ui/format';
import { formatAgeGroup } from '@/ui/csts';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationCompetitorDocument } from '@/graphql/Federation';

type Props = {
  params: Promise<{ federation: string; id: string }>;
};

const getCompetitor = cache(async (federation: string, id: string) => {
  if (!/^[a-zA-Z0-9-]{1,64}$/.test(id)) notFound();
  const { federationCompetitor: competitor } = await executeGraphql(
    FederationCompetitorDocument,
    { id: `${federation}:${id}` },
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

  return (
    <Layout requireUser>
      <PageHeader
        title={competitor.name || competitor.id}
        breadcrumbLabel="Soutěžící"
        breadcrumbs={[
          { label: 'Soutěžní data', href: '/federated' },
          {
            label: competitor.federationByFederation?.name ?? federation,
            href: `/federated/${federation}?view=competitors`,
          },
        ]}
      />
      <DefinitionList
        items={{
          ID: externalId,
          Typ: competitor.competitorType.toLowerCase(),
          Věk: formatAgeGroup(competitor.ageGroup),
        }}
      />

      <FederationCategoryProgress competitors={[{ ...competitor, federation }]} />

      {competitor.couplesList.length > 0 && (
        <>
          <h2 className="mt-4 mb-2 text-lg font-bold">V klubu</h2>
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
      <Members federation={federation} id={competitor.id} />
      <History federation={federation} id={competitor.id} />
    </Layout>
  );
}
