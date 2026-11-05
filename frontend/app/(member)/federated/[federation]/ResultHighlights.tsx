'use client';

import Link from 'next/link';
import { useState } from 'react';
import type { ResultHighlightFragment } from '@/graphql/Federation';
import { Checkbox } from '@/ui/fields/checkbox';
import { formatCategoryName } from '@/ui/csts';
import { formatDateOnly } from '@/ui/format';

type Entry = ResultHighlightFragment & {
  competition: NonNullable<ResultHighlightFragment['competition']>;
};

export function ResultHighlights({
  results,
}: {
  results: readonly ResultHighlightFragment[];
}) {
  const [groupByCompetitor, setGroupByCompetitor] = useState(false);
  const entries = results
    .filter((result): result is Entry => result.competition != null)
    .toSorted((a, b) => b.competition.startDate.localeCompare(a.competition.startDate));
  if (entries.length === 0) return null;
  const competitors = Map.groupBy(entries, (x) => x.competitorId);
  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Významné výsledky</h2>
      {competitors.size > 1 && (
        <Checkbox
          label="Rozdělit podle soutěžících"
          checked={groupByCompetitor}
          onChange={() => setGroupByCompetitor((x) => !x)}
        />
      )}
      {(groupByCompetitor ? [...competitors.values()] : [entries]).map((entries) => {
        const { competitor, competitorId, competition } = entries[0]!;
        return (
          <section key={competitorId} className="space-y-2 mb-4">
            {groupByCompetitor && (
              <h3 className="font-semibold">
                {competitor ? (
                  <Link
                    href={`/federated/${competition.federation}/competitors/${competitor.externalId}`}
                    className="text-accent-11 underline"
                    prefetch={false}
                  >
                    {competitor.name || competitorId}
                  </Link>
                ) : (
                  competitorId
                )}
              </h3>
            )}
            <HighlightList results={entries} />
          </section>
        );
      })}
    </>
  );
}

function HighlightList({ results }: { results: readonly Entry[] }) {
  const entries = results.map((result) => {
    const ranking = result.rankingTo ?? result.ranking;
    const placement = `${result.ranking}${result.rankingTo && result.rankingTo !== result.ranking ? `–${result.rankingTo}` : ''}. místo`;
    if (ranking <= 3)
      return { ...result, placement, label: placement, order: result.ranking };
    const finalIndex = result.competition.finalRound[0]?.roundIndex;
    const reachedIndex = result.round?.roundIndex;
    let depth = Math.max(0, Math.ceil(Math.log2(ranking / 6)));
    if (result.isFinal || result.round?.roundKey === 'F') depth = 0;
    else if (finalIndex != null && reachedIndex != null && finalIndex >= reachedIndex)
      depth = finalIndex - reachedIndex;
    const label =
      ['finalista', 'semifinalista', 'čtvrtfinalista'][depth] ??
      `1/${2 ** depth} finále`;
    return { ...result, placement, label, order: 4 + depth };
  });
  const groups = Map.groupBy(entries, ({ competition, label }) =>
    JSON.stringify([
      competition.federation,
      competition.category?.id ?? competition.id,
      competition.competitionType,
      label,
    ]),
  );
  const highlights = [...groups]
    .map(([key, results]) => ({ ...results[0]!, key, results }))
    .toSorted(
      (a, b) =>
        Number(b.competition.federation === 'wdsf') -
          Number(a.competition.federation === 'wdsf') ||
        Number(b.competition.competitionType === 'CHAMPIONSHIP') -
          Number(a.competition.competitionType === 'CHAMPIONSHIP') ||
        a.order - b.order ||
        (a.rankingTo ?? a.ranking) - (b.rankingTo ?? b.ranking) ||
        a.ranking - b.ranking ||
        b.competition.startDate.localeCompare(a.competition.startDate),
    );

  return (
    <ul className="space-y-2 text-sm">
      {highlights.map(({ key, competition, label, results }) => (
        <li key={key}>
          <details>
            <summary className="cursor-pointer">
              <span className="font-medium tabular-nums">{label}</span>
              {' — '}
              {formatCategoryName(competition) || '?'}
              {' ('}
              {results.length > 1 && `${results.length}×, `}
              {[...new Set(results.map((x) => x.competition.startDate.slice(0, 4)))]
                .toSorted()
                .join(', ')}
              )
            </summary>

            <ul className="mt-1 ml-4 space-y-1 border-l border-neutral-4 pl-3">
              {results.map(({ placement, competition, competitionId }) => {
                const event = competition.event;
                const text = [
                  placement,
                  formatDateOnly(competition.startDate),
                  event?.name || event?.location,
                ]
                  .filter(Boolean)
                  .join(' · ');
                return (
                  <li key={competitionId}>
                    {event ? (
                      <Link
                        href={`/federated/${competition.federation}/${event.externalId}/${competition.externalId}`}
                        className="text-accent-11 underline"
                        prefetch={false}
                      >
                        {text}
                      </Link>
                    ) : (
                      text
                    )}
                  </li>
                );
              })}
            </ul>
          </details>
        </li>
      ))}
    </ul>
  );
}
