import Link from 'next/link';
import type { Metadata } from 'next';
import { notFound } from 'next/navigation';
import { cache } from 'react';
import { CompetitionRoundDocument } from '@/graphql/Federation';
import { executeGraphql } from '@/lib/server/graphql';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { formatDateOnly, formatJudgeScore, scoringMethodLabels } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { PageHeader } from '@/ui/TitleBar';
import { Results } from './Results';
import { FederationPersonLink, LocalPersonLinks } from '@/ui/federation-links';

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
      competition.category?.name ?? competitionId,
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
  const scores = Object.groupBy(
    round.judgeScoresList,
    (s) => `${s.danceOrder}:${s.competitorId}:${s.judgePersonId}`,
  );

  return (
    <Layout requireUser>
      <PageHeader
        title={competition.category?.name ?? competitionId}
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
          { label: competition.category?.name ?? competitionId, href },
          { label: round.roundLabel || round.roundKey },
        ]}
      />
      <nav aria-label="Rounds" className="flex flex-wrap gap-2 my-2">
        {competition.roundsList.map((r) => (
          <Link
            key={r.id}
            href={`${href}/rounds/${encodeURIComponent(r.roundKey)}`}
            className={buttonCls({
              variant: r.id === round.id ? 'primary' : 'outline',
              size: 'sm',
            })}
            aria-current={r.id === round.id ? 'page' : undefined}
            prefetch={false}
          >
            {r.roundLabel || r.roundKey}
          </Link>
        ))}
      </nav>
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
      >
        {round.judgeScoresList.length === 0 ? (
          <p>No marks</p>
        ) : (
          round.dancesList.map((dance) => (
            <div key={dance.danceOrder} className="col-full-width overflow-x-auto mb-4">
              <table className="w-full text-left text-sm tabular-nums">
                <caption className="text-left font-bold mb-2">
                  {dance.programDance?.dance?.name || dance.danceCode}
                </caption>
                <thead>
                  <tr>
                    <th scope="col" className="px-2 py-1">
                      start number
                    </th>
                    <th scope="col" className="px-2 py-1">
                      competitor
                    </th>
                    {round.judgesList.map((judge) => (
                      <th
                        key={judge.personJudgeId}
                        scope="col"
                        className="px-2 py-1 text-center"
                        title={judge.personJudge?.canonicalName || judge.personJudgeId}
                      >
                        {judge.judgeLabel || judge.judgeIndex}
                      </th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {round.resultsList.map((result) => (
                    <tr
                      key={result.competitorId}
                      className="border-t border-neutral-4 align-top"
                    >
                      <td className="px-2 py-1">
                        {startNumbers.get(result.competitorId)}
                      </td>
                      <th scope="row" className="min-w-48 px-2 py-1 font-normal">
                        {result.competitor ? (
                          <Link
                            href={`/federated/${federation}/competitors/${result.competitor.externalId}`}
                            className="text-accent-11 underline"
                            prefetch={false}
                          >
                            {result.competitor.name || result.competitorId}
                          </Link>
                        ) : (
                          result.competitorId
                        )}
                      </th>
                      {round.judgesList.map((judge) => (
                        <td
                          key={judge.personJudgeId}
                          className="px-2 py-1 text-center whitespace-nowrap"
                        >
                          {scores[
                            `${dance.danceOrder}:${result.competitor?.externalId}:${judge.personJudge?.externalId}`
                          ]?.map((score) => (
                            <div key={score.component}>
                              {score.component.startsWith('AJS_') && (
                                <>{score.component.slice(4)} </>
                              )}
                              {formatJudgeScore(score)}
                            </div>
                          )) ?? '–'}
                        </td>
                      ))}
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          ))
        )}
      </Results>
      <h2 className="mt-4 mb-2 text-lg font-bold">Judges</h2>
      <dl className="text-sm tabular">
        {round.judgesList.map((judge) => (
          <div key={judge.personJudgeId}>
            <dt>{judge.judgeLabel || judge.judgeIndex}</dt>
            <dd>
              {judge.personJudge ? (
                <>
                  <FederationPersonLink
                    federation={federation}
                    externalId={judge.personJudge.externalId}
                    className="text-accent-11"
                  >
                    {judge.personJudge.canonicalName ||
                      [judge.personJudge.firstName, judge.personJudge.lastName]
                        .filter(Boolean)
                        .join(' ') ||
                      judge.personJudgeId}
                  </FederationPersonLink>
                  {judge.personJudge.people.totalCount > 0 && (
                    <span className="ml-2 text-sm">
                      Local:{' '}
                      <LocalPersonLinks
                        federation={federation}
                        externalId={judge.personJudge.externalId}
                        people={judge.personJudge.people}
                      />
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
