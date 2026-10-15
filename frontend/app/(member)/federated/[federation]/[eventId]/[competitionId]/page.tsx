import { formatCategoryName } from '@/ui/csts';
import { CstsResultsLink } from '@/ui/federation-links';
import { notFound } from 'next/navigation';
import type { Metadata } from 'next';
import { cache } from 'react';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { Results, Registrations } from './Tables';
import { RoundNav } from './RoundNav';
import { formatDateOnly, dateTimeFormatter } from '@/ui/format';
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
    title: `${formatCategoryName(federation, competition.category, competition.competitionType) || competitionId} · ${formatDateOnly(competition.startDate)}`,
    robots: { index: false, follow: false },
  };
}

export default async function CompetitionPage({ params }: Props) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federation, eventId, competitionId } = await params;
  const { event, competition } = await getCompetition(federation, eventId, competitionId);
  const title =
    formatCategoryName(federation, competition.category, competition.competitionType) ||
    competition.externalId;
  return (
    <Layout requireUser>
      <PageHeader
        title={title}
        subtitle={
          <span className="flex gap-2">
            <span className="shrink-0">{formatDateOnly(competition.startDate)} ·</span>
            <span className="truncate" title={event.name || eventId}>
              {event.name || eventId}
            </span>
          </span>
        }
        breadcrumbs={[
          { label: 'Federations', href: '/federated' },
          {
            label: event.federationByFederation?.name ?? federation,
            href: `/federated/${federation}`,
          },
          {
            label: event.name || event.externalId,
            href: `/federated/${federation}/${eventId}`,
            truncate: true,
          },
          { label: title },
        ]}
      />
      <RoundNav
        rounds={competition.rounds}
        href={`/federated/${federation}/${eventId}/${competitionId}`}
      />
      <dl className="text-sm tabular">
        {federation === 'csts' && (
          <>
            <dt>source</dt>
            <dd>
              <CstsResultsLink
                eventId={event.externalId}
                competitionId={competition.externalId}
              >
                ČSTS results
              </CstsResultsLink>
            </dd>
          </>
        )}
        <dt>status</dt>
        <dd>{competition.status.toLowerCase().replaceAll('_', ' ')}</dd>
        <dt>type</dt>
        <dd>{competition.competitionType?.toLowerCase().replaceAll('_', ' ') || '-'}</dd>
        <dt>check-in end</dt>
        <dd>{competition.checkInEnd?.slice(0, 5) || '-'}</dd>
        <dt>participants reported</dt>
        <dd>{competition.participantsTotal ?? '-'}</dd>
        <dt>completed</dt>
        <dd>
          {competition.completedAt
            ? dateTimeFormatter.format(new Date(competition.completedAt))
            : '-'}
        </dd>
      </dl>
      <Results federation={federation} results={competition.results} />
      <Registrations
        federation={federation}
        entries={competition.entries}
        hasResults={competition.results.length > 0}
      />
      <Officials
        federation={federation}
        officials={competition.officials.nodes}
        showJudges={
          event.status !== 'COMPLETED' &&
          competition.status !== 'COMPLETED' &&
          competition.results.length === 0 &&
          competition.rounds.length === 0
        }
      />
      {auth.isSystemAdmin && (
        <Sources federation={federation} condition={{ competitionId: competition.id }} />
      )}
    </Layout>
  );
}
