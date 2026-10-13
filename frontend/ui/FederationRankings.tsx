import type { FederationRankingFragment } from '@/graphql/Federation';
import { cstsAgeCategories, formatCstsClass } from '@/ui/csts';
import { formatDateOnly } from '@/ui/format';

const disciplineNames: Record<string, string> = {
  Standard: 'STT',
  Latin: 'LAT',
  TenDance: '10T',
};
const disciplineOrder = Object.keys(disciplineNames);

function ranklistRows(entries: FederationRankingFragment[]) {
  return entries
    .flatMap((entry) => {
      const snapshot = entry.snapshot;
      const ranklist = snapshot?.ranklist;
      const category = ranklist?.category;
      if (!snapshot || !ranklist || !category || snapshot.kind === 'Nomination')
        return [];
      return [
        {
          id: `${entry.snapshotId}:${entry.competitorId}`,
          competitor: entry.competitor?.name,
          ranklist: ranklist.name,
          federation:
            ranklist.federation === 'csts' ? 'ČSTS' : ranklist.federation.toUpperCase(),
          date: snapshot.asOfDate,
          kind: snapshot.kind,
          ageGroup: category.ageGroup,
          category: [
            cstsAgeCategories.find((age) => age.id === category.ageGroup)?.name ??
              category.ageGroup,
            formatCstsClass(category.class),
          ]
            .filter(Boolean)
            .join(' '),
          discipline: category.discipline,
          placement: `${entry.ranking}${entry.rankingTo && entry.rankingTo !== entry.ranking ? `–${entry.rankingTo}` : ''}/${snapshot.ranklistEntriesBySnapshotId.totalCount}`,
        },
      ];
    })
    .toSorted(
      (a, b) =>
        cstsAgeCategories.findIndex((age) => age.id === a.ageGroup) -
          cstsAgeCategories.findIndex((age) => age.id === b.ageGroup) ||
        disciplineOrder.indexOf(a.discipline) - disciplineOrder.indexOf(b.discipline),
    );
}

export function FederationRankBadge({
  entries,
  ageGroup,
}: {
  entries: FederationRankingFragment[];
  ageGroup: string | undefined;
}) {
  const rows = ranklistRows(entries).filter((row) => row.ageGroup === ageGroup);
  if (rows.length === 0) return null;
  return (
    <span className="inline-flex flex-wrap gap-x-1 rounded-full bg-accent-3 px-2 py-0.5">
      <span title="Ranklist">RL</span>
      {rows.map((row, index) => (
        <span
          key={row.id}
          className="whitespace-nowrap"
          title={`${row.competitor} · ${row.ranklist} · ${formatDateOnly(row.date)}`}
        >
          {index > 0 && <span className="mr-1">·</span>}
          {disciplineNames[row.discipline] ?? row.discipline} {row.placement}
        </span>
      ))}
    </span>
  );
}

export function FederationRankings({
  entries,
}: {
  entries: FederationRankingFragment[];
}) {
  const snapshots = Object.groupBy(
    ranklistRows(entries),
    (row) => `${row.federation}:${row.kind}:${row.date}`,
  );
  return Object.entries(snapshots).map(([key, rows]) => {
    if (!rows) return null;
    const columns = [...new Set(rows.map((row) => row.discipline))].toSorted(
      (a, b) => disciplineOrder.indexOf(a) - disciplineOrder.indexOf(b),
    );
    return (
      <div key={key} className="my-2 overflow-x-auto">
        <table className="w-full text-sm text-left">
          <caption className="mb-2 text-left text-lg font-semibold">
            Ranklist {rows[0]?.federation} ({formatDateOnly(rows[0]!.date)})
          </caption>
          <thead>
            <tr>
              <th scope="col" className="py-1 pr-3 font-medium">
                Kategorie
              </th>
              {columns.map((discipline) => (
                <th
                  key={discipline}
                  scope="col"
                  className="px-2 py-1 text-right font-medium"
                >
                  {disciplineNames[discipline] ?? discipline}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {Object.entries(Object.groupBy(rows, (row) => row.category)).map(
              ([category, entries]) => (
                <tr key={category} className="border-t border-neutral-4">
                  <th scope="row" className="py-1 pr-3 font-normal">
                    {category}
                  </th>
                  {columns.map((discipline) => (
                    <td key={discipline} className="px-2 py-1 text-right">
                      {entries
                        ?.filter((entry) => entry.discipline === discipline)
                        .map((entry) => (
                          <div key={entry.id}>{entry.placement}</div>
                        ))}
                    </td>
                  ))}
                </tr>
              ),
            )}
          </tbody>
        </table>
      </div>
    );
  });
}
