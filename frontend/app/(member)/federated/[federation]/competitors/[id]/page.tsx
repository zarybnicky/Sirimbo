import { DefinitionList } from '@/ui/DefinitionList';
import { FederationCategoryProgress } from '@/ui/FederationCategoryProgress';
import Link from 'next/link';
import { notFound } from 'next/navigation';
import type { Metadata } from 'next';
import { cache } from 'react';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/PageHeader';
import { History } from './Tables';
import { ResultHighlights } from '../../ResultHighlights';
import { formatDateOnly, formatOpenDateRange } from '@/ui/format';
import { formatAgeGroup, formatCompetitorType } from '@/ui/csts';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationCompetitorDocument } from '@/graphql/Federation';

type Props = {
  params: Promise<{ federation: string; id: string }>;
};

const getCompetitor = cache(async (federation: string, id: string) => {
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
  const today = new Date().toISOString().slice(0, 10);

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
          Typ: formatCompetitorType(competitor.competitorType),
          Věk: formatAgeGroup(competitor.ageGroup),
          'Poslední soutěž':
            (competitor.latestResultAt ? formatDateOnly(competitor.latestResultAt) : '') +
            (competitor.latestEntryAt && competitor.latestEntryAt > today
              ? ' (+ budoucí)'
              : ''),
          Osoby:
            competitor.components.nodes.length > 0 &&
            competitor.components.nodes.map(({ personId, person, role }) => (
              <div key={personId}>
                {person ? (
                  <Link
                    className="text-accent-11 underline"
                    href={`/federated/${federation}/people/${person.externalId}`}
                  >
                    {person.canonicalName ||
                      [person.firstName, person.lastName].filter(Boolean).join(' ') ||
                      personId}
                  </Link>
                ) : (
                  personId
                )}
                {role === 'SUBSTITUTE' && ' (náhradník)'}
              </div>
            )),
          'V klubu':
            competitor.couplesList.length > 0 &&
            competitor.couplesList.map((couple) => (
              <div key={couple.id}>
                <Link className="text-accent-11 underline" href={`/pary/${couple.id}`}>
                  {couple.name}
                </Link>{' '}
                ({formatOpenDateRange(couple)})
              </div>
            )),
        }}
      />

      <FederationCategoryProgress competitors={[competitor]} />

      <ResultHighlights results={competitor.highlights} />
      <History federation={federation} id={competitor.id} />
    </Layout>
  );
}
