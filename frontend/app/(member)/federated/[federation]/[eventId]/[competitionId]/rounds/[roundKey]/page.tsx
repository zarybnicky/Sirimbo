import { formatCategoryName } from '@/ui/csts';
import type { Metadata } from 'next';
import { notFound } from 'next/navigation';
import { cache } from 'react';
import { CompetitionRoundDocument } from '@/graphql/Federation';
import { executeGraphql } from '@/lib/server/graphql';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { formatDateOnly, scoringMethodLabels } from '@/ui/format';
import { RoundNav } from '../../RoundNav';
import { PageHeader } from '@/ui/TitleBar';
import { Results } from './Results';
import { FederationLink, LocalPersonLinks } from '@/ui/federation-links';

type Props = {
  params: Promise<{
    federation: string;
    eventId: string;
    competitionId: string;
    roundKey: string;
  }>;
};

const getRound = cache((federation: string, competitionId: string, roundKey: string) =>
  executeGraphql(CompetitionRoundDocument, { federation, competitionId, roundKey }),
);

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return { robots: { index: false, follow: false } };
  const { federation, eventId, competitionId, roundKey } = await params;
  const { competition } = await getRound(federation, competitionId, roundKey);
  const round = competition?.round[0];
  if (!competition || competition.event?.externalId !== eventId || !round) notFound();

  return {
    title: [
      formatCategoryName(federation, competition.category, competition.competitionType) ||
        competitionId,
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
  const { competition } = await getRound(federation, competitionId, roundKey);
  const round = competition?.round[0];
  if (!competition || competition.event?.externalId !== eventId || !round) notFound();
  const href = `/federated/${federation}/${eventId}/${competitionId}`;
  const hasNextRound =
    competition.roundsList.findIndex((r) => r.id === round.id) <
    competition.roundsList.length - 1;
  const startNumbers = new Map(
    competition.resultsList.map((r) => [r.competitorId, r.startNumber]),
  );

  const title =
    formatCategoryName(federation, competition.category, competition.competitionType) ||
    competitionId;
  return (
    <Layout requireUser>
      <PageHeader
        title={title}
        subtitle={
          <span className="flex gap-2">
            <span className="shrink-0">{formatDateOnly(competition.startDate)} ·</span>
            <span className="truncate" title={competition.event.name || eventId}>
              {competition.event.name || eventId}
            </span>
          </span>
        }
        breadcrumbs={[
          { label: 'Federations', href: '/federated' },
          {
            label: competition.event.federationByFederation?.name ?? federation,
            href: `/federated/${federation}`,
          },
          {
            label: competition.event.name || eventId,
            href: `/federated/${federation}/${eventId}`,
            truncate: true,
          },
          { label: title, href },
          { label: round.roundLabel || round.roundKey },
        ]}
      />
      <RoundNav href={href} rounds={competition.roundsList} current={round.roundKey} />
      <dl className="text-sm tabular">
        <dt>scoring method</dt>
        <dd>{scoringMethodLabels[round.scoringMethod]}</dd>
      </dl>
      <Results
        key={round.id}
        federation={federation}
        round={round}
        startNumbers={startNumbers}
        hasNextRound={hasNextRound}
      />
      <h2 className="mt-4 mb-2 text-lg font-bold">Judges</h2>
      <dl className="text-sm tabular">
        {round.judgesList.map((judge) => (
          <div key={judge.personJudgeId}>
            <dt>{judge.judgeLabel || judge.judgeIndex}</dt>
            <dd>
              {judge.personJudge ? (
                <>
                  <FederationLink
                    entity="people"
                    federation={federation}
                    externalId={judge.personJudge.externalId}
                    className="text-accent-11"
                  >
                    {judge.personJudge.canonicalName ||
                      [judge.personJudge.firstName, judge.personJudge.lastName]
                        .filter(Boolean)
                        .join(' ') ||
                      judge.personJudgeId}
                  </FederationLink>
                  {judge.personJudge.peopleList.length > 0 && (
                    <span className="ml-2 text-sm">
                      Local: <LocalPersonLinks people={judge.personJudge.peopleList} />
                    </span>
                  )}
                </>
              ) : (
                judge.personJudgeId
              )}
            </dd>
          </div>
        ))}
      </dl>
      {round.judgesList.length === 0 && <p>No judging panel</p>}
    </Layout>
  );
}
