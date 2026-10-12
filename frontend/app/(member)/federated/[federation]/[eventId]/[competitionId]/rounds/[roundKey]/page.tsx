import Link from 'next/link';
import { notFound } from 'next/navigation';
import { CompetitionRoundDocument } from '@/graphql/Federation';
import { executeGraphql } from '@/lib/server/graphql';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { buttonCls } from '@/ui/style';
import { PageHeader } from '@/ui/TitleBar';

export const metadata = {
  title: 'Competition round',
  robots: { index: false, follow: false },
};

export default async function RoundPage({
  params,
}: {
  params: Promise<{
    federation: string;
    eventId: string;
    competitionId: string;
    roundKey: string;
  }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federation, eventId, competitionId, roundKey } = await params;
  const { competition } = await executeGraphql(CompetitionRoundDocument, {
    federation,
    competitionId,
    roundKey,
  });
  const round = competition?.round[0];
  if (!competition || competition.event?.externalId !== eventId || !round) notFound();
  const href = `/federated/${federation}/${eventId}/${competitionId}`;
  const hasNextRound =
    competition.roundsList.findIndex((r) => r.id === round.id) <
    competition.roundsList.length - 1;
  const showDanceResults =
    federation === 'csts' || round.scoringMethod === 'SKATING_MARKS';
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
        title={round.roundLabel || round.roundKey}
        breadcrumbs={[
          { label: 'Federations', href: '/federated' },
          {
            label: competition.event.federationByFederation?.name ?? federation,
            href: `/federated/${federation}`,
          },
          {
            label: competition.event.name || eventId,
            href: `/federated/${federation}/${eventId}`,
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
        <dd>
          {
            { SKATING_MARKS: 'Marks', SKATING_PLACES: 'Skating', AJS_3_0: 'AJS 3.0' }[
              round.scoringMethod
            ]
          }
        </dd>
      </dl>
      <h2 className="mt-4 mb-2 text-lg font-bold">Results</h2>
      <div className="col-full-width overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {['rank', 'start number', 'competitor'].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
              {showDanceResults &&
                round.dancesList.map((dance) => (
                  <th
                    key={dance.danceOrder}
                    scope="col"
                    className="px-2 py-1"
                    title={dance.programDance?.dance?.name}
                  >
                    {dance.danceCode}
                  </th>
                ))}
              {showDanceResults && (
                <th scope="col" className="px-2 py-1">
                  total
                </th>
              )}
              {hasNextRound && (
                <th scope="col" className="px-2 py-1">
                  qualified
                </th>
              )}
            </tr>
          </thead>
          <tbody>
            {round.resultsList.map((result) => (
              <tr key={result.competitorId} className="border-t border-neutral-4">
                <td className="px-2 py-1 whitespace-nowrap">
                  {result.overallRanking ?? '–'}
                  {result.overallRankingTo &&
                  result.overallRankingTo !== result.overallRanking
                    ? `–${result.overallRankingTo}`
                    : ''}
                </td>
                <td className="px-2 py-1">{startNumbers.get(result.competitorId)}</td>
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
                {showDanceResults &&
                  round.dancesList.map((dance, index) => (
                    <td key={dance.danceOrder} className="px-2 py-1">
                      {result.danceResults?.[index] ?? '–'}
                    </td>
                  ))}
                {showDanceResults && (
                  <td className="px-2 py-1">
                    {result.overallScore == null ? '–' : Number(result.overallScore)}
                  </td>
                )}
                {hasNextRound && (
                  <td className="px-2 py-1">
                    {result.qualifiedNext == null
                      ? '–'
                      : result.qualifiedNext
                        ? 'yes'
                        : 'no'}
                  </td>
                )}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      {round.resultsList.length === 0 && <p>No round results</p>}
      <h2 className="mt-4 mb-2 text-lg font-bold">Judges</h2>
      <dl className="text-sm tabular">
        {round.judgesList.map((judge) => (
          <div key={judge.personJudgeId}>
            <dt>{judge.judgeLabel || judge.judgeIndex}</dt>
            <dd>
              {judge.personJudge ? (
                <Link
                  href={`/federated/${federation}/people/${judge.personJudge.externalId}`}
                  className="text-accent-11 underline"
                  prefetch={false}
                >
                  {judge.personJudge.canonicalName || judge.personJudgeId}
                </Link>
              ) : (
                judge.personJudgeId
              )}
            </dd>
          </div>
        ))}
      </dl>
      {round.judgesList.length === 0 && <p>No judging panel</p>}
      <h2 className="mt-4 mb-2 text-lg font-bold">Marks</h2>
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
                    <td className="px-2 py-1">{startNumbers.get(result.competitorId)}</td>
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
                            {score.rawScore ??
                              (score.component === 'MARK'
                                ? Number(score.score)
                                  ? '×'
                                  : ''
                                : Number(score.score))}
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
    </Layout>
  );
}
