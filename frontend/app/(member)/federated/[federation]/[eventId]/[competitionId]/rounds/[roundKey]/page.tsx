import { DotSeparated } from '@/ui/DotSeparated';
import { DefinitionList } from '@/ui/DefinitionList';
import { formatCategoryName, formatJudgeLabel } from '@/ui/csts';
import type { Metadata } from 'next';
import { notFound } from 'next/navigation';
import { cache, type ReactNode } from 'react';
import { CompetitionRoundDocument } from '@/graphql/Federation';
import { executeGraphql } from '@/lib/server/graphql';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { formatDateOnly, scoringMethodLabels } from '@/ui/format';
import { RoundNav } from '../../RoundNav';
import { PageHeader } from '@/ui/PageHeader';
import { Results } from './Results';
import { OfficialName } from '../../../Officials';

type Props = {
  params: Promise<{
    federation: string;
    eventId: string;
    competitionId: string;
    roundKey: string;
  }>;
};

const getRound = cache(
  async (federation: string, eventId: string, competitionId: string, roundKey: string) => {
    const { competition } = await executeGraphql(CompetitionRoundDocument, {
      federation,
      competitionId,
      roundKey,
    });
    const event = competition?.event;
    const round = competition?.round[0];
    if (!competition || !event || event.externalId !== eventId || !round) notFound();
    return { competition, event, round };
  },
);

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return { robots: { index: false, follow: false } };
  const { federation, eventId, competitionId, roundKey } = await params;
  const { competition, round } = await getRound(federation, eventId, competitionId, roundKey);

  return {
    title: [
      formatCategoryName(competition) || competitionId,
      round.roundLabel || round.roundKey,
      formatDateOnly(competition.startDate),
    ].join(' · '),
    robots: { index: false, follow: false },
  };
}

export default async function RoundPage({ params }: Props) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;

  const { federation, eventId, competitionId, roundKey } = await params;
  const { competition, event, round } = await getRound(federation, eventId, competitionId, roundKey);

  return (
    <Layout requireUser>
      <PageHeader
        title={formatCategoryName(competition) || competitionId}
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
          { label: 'Soutěžní data', href: '/federated' },
          {
            label: event.federationByFederation?.name ?? federation,
            href: `/federated/${federation}`,
          },
          {
            label: event.name || eventId,
            href: `/federated/${federation}/${eventId}`,
          },
        ]}
      />
      <RoundNav
        href={`/federated/${federation}/${eventId}/${competitionId}`}
        rounds={competition.roundsList}
        current={round.roundKey}
      />

      <DefinitionList
        items={{ 'Typ hodnocení': scoringMethodLabels[round.scoringMethod] }}
      />

      <Results
        key={round.id}
        federation={federation}
        round={round}
        startNumbers={new Map(competition.resultsList.map((r) => [r.competitorId, r.startNumber]))}
        hasNextRound={
          competition.roundsList.findIndex((r) => r.id === round.id) <
          competition.roundsList.length - 1
        }
      />

      <h2 className="mt-4 text-lg font-bold">Porota</h2>
      <DefinitionList
        items={round.judgesList.map((judge): [string, ReactNode] => [
          formatJudgeLabel(judge),
          <OfficialName
            key={judge.personJudgeId}
            official={{ personId: judge.personJudgeId, person: judge.personJudge }}
            compact={false}
          />,
        ])}
      />
    </Layout>
  );
}
