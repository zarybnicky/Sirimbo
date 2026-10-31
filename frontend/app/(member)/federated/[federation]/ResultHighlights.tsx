import Link from 'next/link';
import type { ResultHighlightFragment } from '@/graphql/Federation';
import { formatCategoryName } from '@/ui/csts';
import { formatDateOnly } from '@/ui/format';

export function ResultHighlights({
  results,
}: {
  results: readonly ResultHighlightFragment[];
}) {
  const entries = results.flatMap((result) =>
    result.competition ? [{ ...result, competition: result.competition }] : [],
  );
  const highlights = Map.groupBy(entries, ({ competition, ranking, rankingTo }) =>
    JSON.stringify([
      competition.federation,
      competition.category?.id ?? competition.id,
      competition.competitionType,
      ranking,
      rankingTo ?? ranking,
    ]),
  );

  if (highlights.size === 0) return null;

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Významné výsledky</h2>

      <ul className="space-y-2 text-sm">
        {[...highlights].map(([key, achievements]) => {
          const { competition, ranking, rankingTo } = achievements[0]!;
          return (
            <li key={key}>
              <details>
                <summary className="cursor-pointer">
                  <span className="font-medium tabular-nums">
                    {ranking}
                    {rankingTo && rankingTo !== ranking ? `–${rankingTo}` : ''}. místo
                  </span>
                  {' — '}
                  {formatCategoryName(
                    competition.federation,
                    competition.category,
                    competition.competitionType,
                  ) || 'Neznámá kategorie'}{' '}
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
