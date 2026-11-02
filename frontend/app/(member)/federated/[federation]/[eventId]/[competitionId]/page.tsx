import { DotSeparated } from '@/ui/DotSeparated';
import { DefinitionList } from '@/ui/DefinitionList';
import { federationStatusLabels, formatCategoryName } from '@/ui/csts';
import { CstsRegistrationLink, CstsResultsLink } from '@/ui/federation-links';
import { notFound } from 'next/navigation';
import type { Metadata } from 'next';
import { cache } from 'react';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/PageHeader';
import { Registrations, Results } from './Tables';
import { RoundNav } from './RoundNav';
import { dateTimeFormatter, formatDateOnly } from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationCompetitionDocument } from '@/graphql/Federation';
import { Sources } from '../../Sources';
import { Officials } from '../Officials';

type Props = {
  params: Promise<{ federation: string; eventId: string; competitionId: string }>;
};

const getCompetition = cache(
  async (federation: string, eventId: string, competitionId: string) => {
    const { competition } = await executeGraphql(FederationCompetitionDocument, {
      federation,
      competitionId,
    });
    const event = competition?.event;
    if (!competition || !event || event.externalId !== eventId) notFound();
    return { event, competition };
  },
);

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return { robots: { index: false, follow: false } };
  const { federation, eventId, competitionId } = await params;
  const { competition } = await getCompetition(federation, eventId, competitionId);
  return {
    title: `${formatCategoryName(competition) || competitionId} · ${formatDateOnly(competition.startDate)}`,
    robots: { index: false, follow: false },
  };
}

export default async function CompetitionPage({ params }: Props) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;

  const { federation, eventId, competitionId } = await params;
  const { event, competition } = await getCompetition(federation, eventId, competitionId);

  return (
    <Layout requireUser>
      <PageHeader
        title={formatCategoryName(competition) || competition.externalId}
        subtitle={
          <DotSeparated className="flex">
            <span className="shrink-0">{formatDateOnly(competition.startDate)}</span>
            <span className="truncate" title={event.name || eventId}>
              {event.name || eventId}
            </span>
          </DotSeparated>
        }
        breadcrumbLabel="Soutěž"
        breadcrumbs={[
          { label: 'Organizace', href: '/federated' },
          {
            label: event.federationByFederation?.name ?? federation,
            href: `/federated/${federation}`,
          },
          {
            label: event.name || event.externalId,
            href: `/federated/${federation}/${eventId}`,
          },
        ]}
      />
      <RoundNav
        rounds={competition.rounds}
        href={`/federated/${federation}/${eventId}/${competitionId}`}
      />

      <DefinitionList
        className="text-sm"
        items={{
          Zdroj: federation === 'csts' && (
            <CstsResultsLink
              eventId={event.externalId}
              competitionId={competition.externalId}
            >
              Výsledky
            </CstsResultsLink>
          ),
          Přihlášení: federation === 'csts' && event.extraId && (
            <CstsRegistrationLink
              eventId={event.externalId}
              eventCompetitionId={event.extraId}
            >
              Přihlásit se
            </CstsRegistrationLink>
          ),
          Stav: federationStatusLabels[competition.status],
          'Konec prezence': competition.checkInEnd?.slice(0, 5),
          Účastníků: competition.participantsTotal ?? competition.results.length,
          'Dokončena v':
            competition.completedAt &&
            dateTimeFormatter.format(new Date(competition.completedAt)),
        }}
      />

      <Results federation={federation} results={competition.results} />
      <Registrations
        federation={federation}
        entries={competition.entries}
        hasResults={competition.results.length > 0}
      />
      <Officials
        federation={federation}
        officials={competition.officials.nodes}
        showJudges
      />
      <Sources federation={federation} condition={{ competitionId: competition.id }} />
    </Layout>
  );
}
