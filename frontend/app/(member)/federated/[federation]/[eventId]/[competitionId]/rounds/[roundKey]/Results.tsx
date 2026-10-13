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
  const scores = useMemo(() => {
    const activeComponents = new Set(
      round.judgeScoresList
        .filter((score) => Number(score.score) !== 0)
        .map((score) => `${score.danceOrder}:${score.component}`),
    );
    return Object.groupBy(
      round.judgeScoresList.filter((score) =>
        score.component === 'AJS_REDUCTION'
          ? Number(score.score) !== 0
          : !score.component.startsWith('AJS_') ||
            activeComponents.has(`${score.danceOrder}:${score.component}`),
      ),
      (s): string => `${s.competitorId}:${s.danceOrder}:${s.judgePersonId}`,
    );
  }, [round.judgeScoresList]);
  const dances = round.dancesList.map((dance) => ({
    key: String(dance.danceOrder),
    label: dance.danceCode,
    title: dance.programDance?.dance?.name,
  }));
  const judges = round.judgesList.map((judge) => ({
    key: judge.personJudge?.externalId ?? judge.personJudgeId,
    label: judge.judgeLabel || judge.judgeIndex,
    title: judge.personJudge?.canonicalName ?? undefined,
    personId: judge.personJudgeId,
  }));
  const groups = showMarks ? (view === 'dance' ? dances : judges) : [];
  const columns = groups.flatMap((group) =>
    (view === 'dance' ? judges : dances).map((column, index) => ({
      ...column,
      key: view === 'dance' ? `${group.key}:${column.key}` : `${column.key}:${group.key}`,
      border: index === 0,
    })),
  );
  const rows = round.resultsList.map((result) => ({
    ...result,
    startNumber: startNumbers.get(result.competitorId),
    competitorLink: result.competitor ? (
      <Link
        href={`/federated/${federation}/competitors/${result.competitor.externalId}`}
        className="text-accent-11 underline"
        prefetch={false}
      >
        {result.competitor.name || result.competitorId}
      </Link>
    ) : (
      result.competitorId
    ),
  }));
  const resultRowSpan = showMarks && showDanceResults ? 2 : 1;

  function scoreCells(
    competitorId: string | undefined,
    cells: { key: string; border?: boolean }[],
    danceKey?: string,
  ) {
    return cells.map((column) => {
      const key = danceKey == null ? column.key : `${danceKey}:${column.key}`;
      const marks = scores[`${competitorId}:${key}`];
      return (
        <td
          key={column.key}
          className={cn(
            'text-center whitespace-nowrap',
            showMarks && 'px-0.5 py-1 font-mono',
            column.border && 'border-l border-neutral-4',
          )}
        >
          {view === 'detailed'
            ? (marks?.map((score) => (
                <div key={score.component}>
                  {score.component.startsWith('AJS_') && <>{score.component.slice(4)} </>}
                  {formatJudgeScore(score)}
                </div>
              )) ?? '–')
            : formatJudgeScore(marks?.[0])}
        </td>
      );
    });
  }

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
                  colSpan={view === 'dance' ? judges.length : dances.length}
                  className="border-l border-neutral-4 px-2 py-1 text-center"
                  title={group.title}
                >
                  {group.label}
                </th>
              ))}
              {!showMarks &&
                showDanceResults &&
                dances.map((dance) => (
                  <th
                    key={dance.key}
                    scope="col"
                    className="px-2 py-1"
                    title={dance.title}
                  >
                    {dance.label}
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
                {columns.map((column) => (
                  <th
                    key={column.key}
                    scope="col"
                    className={cn(
                      'px-0.5 py-1 text-center font-mono font-normal',
                      column.border && 'border-l border-neutral-4',
                    )}
                    title={column.title}
                  >
                    {column.label}
                  </th>
                ))}
              </tr>
            )}
          </thead>
          {rows.map((result) => (
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
                  {result.startNumber}
                </td>
                <th
                  scope="rowgroup"
                  rowSpan={resultRowSpan}
                  className="min-w-48 px-2 py-1 font-normal"
                >
                  {result.competitorLink}
                </th>
                {scoreCells(result.competitor?.externalId, columns)}
                {!showMarks &&
                  showDanceResults &&
                  dances.map((dance, index) => (
                    <td key={dance.key} className="px-2 py-1">
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
                    dances.map((dance, index) => (
                      <td
                        key={dance.key}
                        colSpan={judges.length}
                        className="border-l border-t border-neutral-4 px-2 py-0.5 text-center font-medium"
                      >
                        {result.danceResults?.[index] ?? '–'}
                      </td>
                    ))
                  ) : (
                    <td
                      colSpan={dances.length * judges.length}
                      className="border-l border-t border-neutral-4 px-2 py-0.5"
                    >
                      <dl
                        aria-label="Dance results"
                        className="flex flex-wrap justify-center gap-x-4"
                      >
                        {dances.map((dance, index) => (
                          <div key={dance.key} className="flex gap-1">
                            <dt>{dance.label}</dt>
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
      {rows.length === 0 && <p>No round results</p>}
      {view === 'detailed' &&
        (round.judgeScoresList.length === 0 ? (
          <p>No marks</p>
        ) : (
          dances.map((dance) => (
            <Table
              key={dance.key}
              className="col-feature mb-4"
              caption={dance.title || dance.label}
              columns={[
                'start number',
                'competitor',
                ...judges.map((judge) => (
                  <span
                    key={judge.personId}
                    className="block text-center"
                    title={judge.title || judge.personId}
                  >
                    {judge.label}
                  </span>
                )),
              ]}
            >
              {rows.map((result) => (
                <tr
                  key={result.competitorId}
                  className="border-t border-neutral-4 align-top"
                >
                  <td>{result.startNumber}</td>
                  <th scope="row" className="min-w-48">
                    {result.competitorLink}
                  </th>
                  {scoreCells(result.competitor?.externalId, judges, dance.key)}
                </tr>
              ))}
            </Table>
          ))
        ))}
    </>
  );
}
