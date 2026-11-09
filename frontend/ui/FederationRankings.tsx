import type { FederationRankingFragment } from '@/graphql/Federation';
import {
  compareCategories,
  compareDisciplines,
  formatAgeGroup,
  formatClass,
  formatCompetitorType,
  formatDiscipline,
} from '@/ui/csts';
import { formatDateOnly } from '@/ui/format';
import { Table } from '@/ui/Table';

function ranklistRows(entries: FederationRankingFragment[]) {
  return entries
    .flatMap((entry) => {
      const snapshot = entry.snapshot;
      const ranklist = snapshot?.ranklist;
      if (!snapshot || !ranklist || !ranklist?.category || snapshot.kind === 'Nomination')
        return [];
      const total = snapshot.ranklistEntriesBySnapshotId.totalCount;
      return {
        ...entry,
        snapshot,
        ranklist,
        category: ranklist?.category,
        id: `${entry.snapshotId}:${entry.competitorId}`,
        percentile:
          entry.ranking > 0 && total > 0
            ? (entry.ranking + (entry.rankingTo ?? entry.ranking)) / 2 / total
            : null,
        placement:
          entry.ranking +
          (entry.rankingTo && entry.rankingTo !== entry.ranking
            ? `–${entry.rankingTo}.`
            : '.'),
        total,
      };
    })
    .toSorted((a, b) => compareCategories(a.category, b.category));
}

export function FederationRankBadge({
  entries,
}: {
  entries: FederationRankingFragment[];
}) {
  const groups = Map.groupBy(
    ranklistRows(entries).filter(
      (row) => row.category.ageGroup && row.percentile !== null,
    ),
    (row) => row.category.ageGroup!,
  );
  let rows: ReturnType<typeof ranklistRows> = [];
  let bestCoverage = 0;
  let bestScore = Infinity;
  for (const group of groups.values()) {
    const disciplines = Map.groupBy(group, (row) => row.category.discipline);
    // Count each discipline once, even with results in several ranklists.
    const scores = [...disciplines.values()].map((results) =>
      Math.min(...results.map((row) => row.percentile!)),
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
  const first = rows[0];
  if (!first) return null;
  return (
    <span className="inline-flex self-start flex-wrap gap-x-1 rounded-full bg-accent-3 px-2 py-0.5">
      <span title="Ranklist">RL</span>
      <span>{formatAgeGroup(first.category.ageGroup)}</span>
      {rows.map((row) => (
        <span
          key={row.id}
          className="whitespace-nowrap"
          title={`${row.competitor?.name} · ${row.ranklist.name} · ${formatDateOnly(row.snapshot.asOfDate)}`}
        >
          {formatDiscipline(row.category.discipline)} {row.placement}
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
  const columns = [...new Set(rows.map((row) => row.category.discipline))].toSorted(
    compareDisciplines,
  );
  const groups = Map.groupBy(rows, ({ ranklist, category, snapshot }) =>
    [
      ranklist.federation === 'csts' ? 'ČSTS' : ranklist.federation.toUpperCase(),
      formatAgeGroup(category.ageGroupDetails ?? category.ageGroup),
      category.competitorType !== 'COUPLE' &&
        formatCompetitorType(category.competitorType),
      formatClass(category.class),
      category.series === 'DanceSport'
        ? null
        : category.series === 'DanceForAll'
          ? 'TPV'
          : category.series,
      `(${formatDateOnly(snapshot.asOfDate)})`,
    ]
      .filter(Boolean)
      .join(' '),
  );
  return (
    <Table
      columns={[
        'Ranklist',
        ...columns.map((discipline) => (
          <span key={discipline} className="block text-right">
            {formatDiscipline(discipline)}
          </span>
        )),
      ]}
    >
      {[...groups].map(([label, entries]) => (
        <tr key={label}>
          <th scope="row">{label}</th>
          {columns.map((discipline) => (
            <td key={discipline} className="text-right">
              {entries
                .filter((x) => x.category.discipline === discipline)
                .map((x) => (
                  <div key={x.id}>
                    {x.placement}/{x.total}
                  </div>
                ))}
            </td>
          ))}
        </tr>
      ))}
    </Table>
  );
}
