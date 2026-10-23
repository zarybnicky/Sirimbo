import type { FederationRankingFragment } from '@/graphql/Federation';
import { compareNaturalText } from '@/lib/compare';
import {
  compareCategories,
  compareDisciplines,
  formatAgeGroup,
  formatClass,
} from '@/ui/csts';
import { formatDateOnly } from '@/ui/format';
import { Table } from '@/ui/Table';

const disciplineNames: Record<string, string> = {
  Standard: 'STT',
  Latin: 'LAT',
  TenDance: '10T',
};

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
          competitorType: category.competitorType,
          series: category.series,
          categoryDetails: category,
          category: [
            formatAgeGroup(category.ageGroup, category.ageGroupDetails?.name),
            category.competitorType !== 'COUPLE' && category.competitorType.toLowerCase(),
            formatClass(category.class),
          ]
            .filter(Boolean)
            .join(' '),
          discipline: category.discipline,
          percentile:
            entry.ranking > 0 && snapshot.ranklistEntriesBySnapshotId.totalCount > 0
              ? (entry.ranking + (entry.rankingTo ?? entry.ranking)) /
                2 /
                snapshot.ranklistEntriesBySnapshotId.totalCount
              : null,
          placement:
            entry.ranking +
            (entry.rankingTo && entry.rankingTo !== entry.ranking
              ? `–${entry.rankingTo}.`
              : '.'),
        },
      ];
    })
    .toSorted(
      (a, b) =>
        compareCategories(a.categoryDetails, b.categoryDetails),
    );
}

export function FederationRankBadge({
  entries,
}: {
  entries: FederationRankingFragment[];
}) {
  const types = Object.groupBy(ranklistRows(entries), (row) => row.competitorType);
  return (
    <>
      {Object.entries(types).map(([type, rows]) => (
        <RankBadge key={type} entries={rows!} />
      ))}
    </>
  );
}

function RankBadge({ entries }: { entries: ReturnType<typeof ranklistRows> }) {
  const groups = Object.groupBy(
    entries.filter((row) => row.ageGroup && row.percentile !== null),
    (row) => row.ageGroup!,
  );
  let rows: ReturnType<typeof ranklistRows> = [];
  let bestCoverage = 0;
  let bestScore = Infinity;
  for (const group of Object.values(groups)) {
    if (!group) continue;
    const disciplines = Object.groupBy(group, (row) => row.discipline);
    // Count each discipline once, even with results in several ranklists.
    const scores = Object.values(disciplines).map((results) =>
      Math.min(...results!.map((row) => row.percentile!)),
    );
    const score = scores.reduce((sum, value) => sum + value, 0) / scores.length;
    if (
      scores.length > bestCoverage ||
      (scores.length === bestCoverage && score < bestScore)
    ) {
      rows = group;
      bestCoverage = scores.length;
      bestScore = score;
    }
  }
  if (rows.length === 0) return null;
  return (
    <span className="inline-flex flex-wrap gap-x-1 rounded-full bg-accent-3 px-2 py-0.5">
      <span title="Ranklist">RL</span>
      {rows[0] && <span>{formatAgeGroup(rows[0].ageGroup)}</span>}
      {rows[0] && rows[0].competitorType !== 'COUPLE' && (
        <span>{rows[0].competitorType.toLowerCase()}</span>
      )}
      {rows.map((row) => (
        <span
          key={row.id}
          className="whitespace-nowrap"
          title={`${row.competitor} · ${row.ranklist} · ${formatDateOnly(row.date)}`}
        >
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
  const rows = ranklistRows(entries);
  if (rows.length === 0) return null;
  const columns = [...new Set(rows.map((row) => row.discipline))].toSorted(
    (a, b) => compareDisciplines(a, b) || compareNaturalText(a, b),
  );
  const groups = Object.groupBy(rows, (row) =>
    [
      row.federation,
      row.category,
      row.series === 'DanceSport'
        ? null
        : row.series === 'DanceForAll'
          ? 'TPV'
          : row.series,
      `(${formatDateOnly(row.date)})`,
    ]
      .filter(Boolean)
      .join(' '),
  );
  return (
    <Table
      className="my-2"
      caption="Ranklist"
      columns={[
        'Kategorie',
        ...columns.map((discipline) => (
          <span key={discipline} className="block text-right">
            {disciplineNames[discipline] ?? discipline}
          </span>
        )),
      ]}
    >
      {Object.entries(groups).map(([label, entries]) => (
        <tr key={label} className="border-t border-neutral-4">
          <th scope="row">{label}</th>
          {columns.map((discipline) => (
            <td key={discipline} className="text-right">
              {entries
                ?.filter((entry) => entry.discipline === discipline)
                .map((entry) => (
                  <div key={entry.id}>{entry.placement}</div>
                ))}
            </td>
          ))}
        </tr>
      ))}
    </Table>
  );
}
