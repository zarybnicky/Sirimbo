'use client';

import Link from 'next/link';
import { Table } from '@/ui/Table';
import { useMemo, useState } from 'react';
import type { CompetitionRoundQuery } from '@/graphql/Federation';
import { cn } from '@/lib/cn';
import { formatJudgeScore } from '@/ui/format';
import { buttonCls, buttonGroupCls } from '@/ui/style';

export function Results({
  federation,
  round,
  startNumbers,
  hasNextRound,
}: {
  federation: string;
  round: NonNullable<CompetitionRoundQuery['competition']>['round'][number];
  startNumbers: Map<string, string | null>;
  hasNextRound: boolean;
}) {
  const showDanceResults =
    federation === 'csts' || round.scoringMethod === 'SKATING_MARKS';
  const canGroupMarks =
    round.scoringMethod !== 'AJS_3_0' &&
    round.judgeScoresList.length > 0 &&
    round.dancesList.length > 0 &&
    round.judgesList.length > 0;
  const [view, setView] = useState<'dance' | 'judge' | 'detailed'>(
    canGroupMarks ? 'dance' : 'detailed',
  );
  const showMarks = canGroupMarks && view !== 'detailed';
  const scores = useMemo(
    () =>
      Object.groupBy(
        round.judgeScoresList,
        (s) => `${s.danceOrder}:${s.competitorId}:${s.judgePersonId}`,
      ),
    [round.judgeScoresList],
  );
  const groups = !showMarks
    ? []
    : view === 'dance'
      ? round.dancesList.map((dance) => ({
          key: dance.danceOrder,
          label: dance.danceCode,
          title: dance.programDance?.dance?.name,
          columns: round.judgesList.map((judge) => ({
            danceOrder: dance.danceOrder,
            judgeId: judge.personJudge?.externalId,
            label: judge.judgeLabel || judge.judgeIndex,
            title: judge.personJudge?.canonicalName ?? undefined,
          })),
        }))
      : round.judgesList.map((judge) => ({
          key: judge.personJudgeId,
          label: judge.judgeLabel || judge.judgeIndex,
          title: judge.personJudge?.canonicalName ?? undefined,
          columns: round.dancesList.map((dance) => ({
            danceOrder: dance.danceOrder,
            judgeId: judge.personJudge?.externalId,
            label: dance.danceCode,
            title: dance.programDance?.dance?.name,
          })),
        }));

  const resultRowSpan = showMarks && showDanceResults ? 2 : 1;

  return (
    <>
      <div className="mt-4 mb-2 flex flex-wrap items-center justify-between gap-2">
        <h2 className="text-lg font-bold">Results</h2>
        {canGroupMarks && (
          <div role="group" aria-label="Results view" className={buttonGroupCls()}>
            {(
              [
                ['dance', 'By dance'],
                ['judge', 'By judge'],
                ['detailed', 'Detailed'],
              ] as const
            ).map(([group, label]) => (
              <button
                key={group}
                type="button"
                aria-pressed={view === group}
                className={buttonCls({
                  variant: view === group ? 'primary' : 'outline',
                  size: 'sm',
                })}
                onClick={() => setView(group)}
              >
                {label}
              </button>
            ))}
          </div>
        )}
      </div>
      <div className="col-feature overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {['rank', 'start number', 'competitor'].map((label) => (
                <th
                  key={label}
                  scope="col"
                  rowSpan={showMarks ? 2 : 1}
                  className="px-2 py-1"
                >
                  {label}
                </th>
              ))}
              {groups.map((group) => (
                <th
                  key={group.key}
                  scope="colgroup"
                  colSpan={group.columns.length}
                  className="border-l border-neutral-4 px-2 py-1 text-center"
                  title={group.title}
                >
                  {group.label}
                </th>
              ))}
              {!showMarks &&
                showDanceResults &&
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
                <th scope="col" rowSpan={showMarks ? 2 : 1} className="px-2 py-1">
                  total
                </th>
              )}
              {hasNextRound && (
                <th scope="col" rowSpan={showMarks ? 2 : 1} className="px-2 py-1">
                  qualified
                </th>
              )}
            </tr>
            {showMarks && (
              <tr>
                {groups.flatMap((group) =>
                  group.columns.map((column, index) => (
                    <th
                      key={`${column.danceOrder}:${column.judgeId}`}
                      scope="col"
                      className={cn(
                        'px-0.5 py-1 text-center font-mono font-normal',
                        index === 0 && 'border-l border-neutral-4',
                      )}
                      title={column.title}
                    >
                      {column.label}
                    </th>
                  )),
                )}
              </tr>
            )}
          </thead>
          {round.resultsList.map((result) => (
            <tbody key={result.competitorId} className="border-t border-neutral-4">
              <tr>
                <td rowSpan={resultRowSpan} className="px-2 py-1 whitespace-nowrap">
                  {result.overallRanking ?? '–'}
                  {result.overallRankingTo &&
                  result.overallRankingTo !== result.overallRanking
                    ? `–${result.overallRankingTo}`
                    : ''}
                </td>
                <td rowSpan={resultRowSpan} className="px-2 py-1">
                  {startNumbers.get(result.competitorId)}
                </td>
                <th
                  scope="rowgroup"
                  rowSpan={resultRowSpan}
                  className="min-w-48 px-2 py-1 font-normal"
                >
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
                {groups.flatMap((group) =>
                  group.columns.map((column, index) => {
                    const score =
                      scores[
                        `${column.danceOrder}:${result.competitor?.externalId}:${column.judgeId}`
                      ]?.[0];
                    return (
                      <td
                        key={`${column.danceOrder}:${column.judgeId}`}
                        className={cn(
                          'px-0.5 py-1 text-center font-mono whitespace-nowrap',
                          index === 0 && 'border-l border-neutral-4',
                        )}
                      >
                        {formatJudgeScore(score)}
                      </td>
                    );
                  }),
                )}
                {!showMarks &&
                  showDanceResults &&
                  round.dancesList.map((dance, index) => (
                    <td key={dance.danceOrder} className="px-2 py-1">
                      {result.danceResults?.[index] ?? '–'}
                    </td>
                  ))}
                {showDanceResults && (
                  <td rowSpan={showMarks ? 2 : 1} className="px-2 py-1">
                    {result.overallScore == null ? '–' : Number(result.overallScore)}
                  </td>
                )}
                {hasNextRound && (
                  <td rowSpan={resultRowSpan} className="px-2 py-1">
                    {result.qualifiedNext == null
                      ? '–'
                      : result.qualifiedNext
                        ? 'yes'
                        : 'no'}
                  </td>
                )}
              </tr>
              {showMarks && showDanceResults && (
                <tr>
                  {view === 'dance' ? (
                    round.dancesList.map((dance, index) => (
                      <td
                        key={dance.danceOrder}
                        colSpan={round.judgesList.length}
                        className="border-l border-t border-neutral-4 px-2 py-0.5 text-center font-medium"
                      >
                        {result.danceResults?.[index] ?? '–'}
                      </td>
                    ))
                  ) : (
                    <td
                      colSpan={round.dancesList.length * round.judgesList.length}
                      className="border-l border-t border-neutral-4 px-2 py-0.5"
                    >
                      <dl
                        aria-label="Dance results"
                        className="flex flex-wrap justify-center gap-x-4"
                      >
                        {round.dancesList.map((dance, index) => (
                          <div key={dance.danceOrder} className="flex gap-1">
                            <dt>{dance.danceCode}</dt>
                            <dd className="font-medium">
                              {result.danceResults?.[index] ?? '–'}
                            </dd>
                          </div>
                        ))}
                      </dl>
                    </td>
                  )}
                </tr>
              )}
            </tbody>
          ))}
        </table>
      </div>
      {round.resultsList.length === 0 && <p>No round results</p>}
      {view === 'detailed' &&
        (round.judgeScoresList.length === 0 ? (
          <p>No marks</p>
        ) : (
          round.dancesList.map((dance) => (
            <Table
              key={dance.danceOrder}
              className="col-feature mb-4"
              caption={dance.programDance?.dance?.name || dance.danceCode}
              columns={[
                'start number',
                'competitor',
                ...round.judgesList.map((judge) => (
                  <span
                    key={judge.personJudgeId}
                    className="block text-center"
                    title={judge.personJudge?.canonicalName || judge.personJudgeId}
                  >
                    {judge.judgeLabel || judge.judgeIndex}
                  </span>
                )),
              ]}
            >
              {round.resultsList.map((result) => (
                <tr
                  key={result.competitorId}
                  className="border-t border-neutral-4 align-top"
                >
                  <td>{startNumbers.get(result.competitorId)}</td>
                  <th scope="row" className="min-w-48">
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
                      className="text-center whitespace-nowrap"
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
            </Table>
          ))
        ))}
    </>
  );
}
