import type { FederationCompetitorQuery } from '@/graphql/Federation';
import { formatCategoryName } from '@/ui/csts';
import { Table } from '@/ui/Table';

export function FederationCategoryProgress({
  competitors,
  singleFederation = false,
}: {
  singleFederation?: boolean;
  competitors: {
    id: string;
    federation: string;
    categoryProgressesList: NonNullable<
      FederationCompetitorQuery['federationCompetitor']
    >['categoryProgressesList'];
  }[];
}) {
  const entries = competitors.flatMap((c) =>
    c.categoryProgressesList.map((x) => ({
      ...x,
      competitorId: c.id,
      federation: c.federation,
    })),
  );
  if (entries.length === 0) return null;

  return (
    <Table columns={['Kategorie', { label: 'Body', colSpan: 2 }]}>
      {entries
        .toSorted(
          (a, b) =>
            a.federation.localeCompare(b.federation) ||
            (formatCategoryName(a.federation, a.category) || a.categoryId).localeCompare(
              formatCategoryName(b.federation, b.category) || b.categoryId,
            ) ||
            a.competitorId.localeCompare(b.competitorId),
        )
        .map((progress) => (
          <tr
            key={`${progress.competitorId}:${progress.categoryId}`}
            className="border-t border-neutral-4"
          >
            <th scope="row">
              {!singleFederation && (
                <>
                  {progress.federation === 'csts'
                    ? 'ČSTS'
                    : progress.federation.toUpperCase()}
                  {' · '}
                </>
              )}
              {formatCategoryName(progress.federation, progress.category)}
            </th>
            <td width="36" className="text-right">
              {Number(progress.points)}b
            </td>
            <td width="24">{progress.domesticFinals + progress.foreignFinals}F</td>
          </tr>
        ))}
    </Table>
  );
}
