'use client';

import Link from 'next/link';
import { useId, useState } from 'react';
import type { ResultHighlightFragment } from '@/graphql/Federation';
import { Checkbox } from '@/ui/fields/checkbox';
import { formatCategoryName } from '@/ui/csts';
import { formatDateOnly } from '@/ui/format';

export function ResultHighlights({
  results,
}: {
  results: readonly ResultHighlightFragment[];
}) {
  const toggleId = useId();
  const [groupByCompetitor, setGroupByCompetitor] = useState(false);
  if (results.length === 0) return null;

  const entries = results.flatMap((result) =>
    result.competition ? [{ ...result, competition: result.competition }] : [],
  );
  const groups = Map.groupBy(
    entries,
    ({ competition, competitorId, ranking, rankingTo }) =>
      JSON.stringify([
        groupByCompetitor ? competitorId : null,
        competition.federation,
        competition.category?.id ?? competition.id,
        competition.competitionType,
        ranking,
        rankingTo ?? ranking,
      ]),
  );
  const highlights = [...groups]
    .map(([key, results]) => {
      results.sort(
        (a, b) =>
          b.competition.startDate.localeCompare(a.competition.startDate) ||
          b.competitionId.localeCompare(a.competitionId, undefined, { numeric: true }) ||
          a.competitorId.localeCompare(b.competitorId),
      );
      return { ...results[0]!, key, results };
    })
    .toSorted(
      (a, b) =>
        Number(b.competition.federation === 'wdsf') -
          Number(a.competition.federation === 'wdsf') ||
        Number(b.competition.competitionType === 'CHAMPIONSHIP') -
          Number(a.competition.competitionType === 'CHAMPIONSHIP') ||
        (a.rankingTo ?? a.ranking) - (b.rankingTo ?? b.ranking) ||
        a.ranking - b.ranking ||
        b.competition.startDate.localeCompare(a.competition.startDate) ||
        a.key.localeCompare(b.key),
    );

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Významné výsledky</h2>
      {new Set(results.map((result) => result.competitorId)).size > 1 && (
        <Checkbox
          name={toggleId}
          label="Rozdělit podle soutěžících"
          checked={groupByCompetitor}
          onChange={(event) => setGroupByCompetitor(event.currentTarget.checked)}
        />
      )}

      <ul className="space-y-2 text-sm">
        {highlights.map((highlight) => {
          const {
            competition: { federation, category, competitionType },
            competitor,
            ranking,
            rankingTo,
          } = highlight;
          const achievements = highlight.results;
          return (
            <li key={highlight.key}>
              <details>
                <summary className="cursor-pointer">
                  {groupByCompetitor && competitor && <>{competitor.name} — </>}
                  <span className="font-medium tabular-nums">
                    {achievements.length}× {ranking}
                    {rankingTo && rankingTo !== ranking ? `–${rankingTo}` : ''}. místo
                  </span>
                  {' — '}
                  {formatCategoryName(federation ?? '', category, competitionType) ||
                    'Neznámá kategorie'}{' '}
                  (
                  {[
                    ...new Set(
                      achievements.map((x) => x.competition.startDate.slice(0, 4)),
                    ),
                  ]
                    .toSorted()
                    .join(', ')}
                  )
                </summary>

                <ul className="mt-1 ml-4 space-y-1 border-l border-neutral-4 pl-3">
                  {achievements.map(
                    ({
                      competitionId,
                      competitorId,
                      competition: { event, startDate, externalId, federation },
                    }) => (
                      <li key={`${competitionId}:${competitorId}`}>
                        {event ? (
                          <Link
                            href={`/federated/${federation}/${event.externalId}/${externalId}`}
                            className="text-accent-11 underline underline-offset-2"
                            prefetch={false}
                          >
                            <time dateTime={startDate}>{formatDateOnly(startDate)}</time>
                            {(event.name || event.location || event.city) &&
                              ` · ${event.name || event.location || event.city}`}
                          </Link>
                        ) : (
                          <time dateTime={startDate}>{formatDateOnly(startDate)}</time>
                        )}
                      </li>
                    ),
                  )}
                </ul>
              </details>
            </li>
          );
        })}
      </ul>
    </>
  );
}
