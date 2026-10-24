import { DotSeparated } from '@/ui/DotSeparated';
import type { FederationCompetitorQuery } from '@/graphql/Federation';
import { compareNaturalText, compareText } from '@/lib/compare';
import { compareCategories, formatCategoryName } from '@/ui/csts';
import { formatPointsAndFinals } from '@/ui/format';
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
    <Table columns={['Kategorie', <span key="points" className="block text-right">Body</span>]}>
      {entries
        .toSorted(
          (a, b) =>
            compareText(a.federation, b.federation) ||
            compareCategories(a.category, b.category) ||
            compareNaturalText(
              formatCategoryName(a.federation, a.category) || a.categoryId,
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
              <DotSeparated>
                {!singleFederation &&
                  (progress.federation === 'csts'
                    ? 'ČSTS'
                    : progress.federation.toUpperCase())}
                {formatCategoryName(progress.federation, progress.category)}
              </DotSeparated>
            </th>
            <td className="whitespace-nowrap text-right">
              {formatPointsAndFinals(
                progress.points,
                progress.domesticFinals + progress.foreignFinals,
              )}
            </td>
          </tr>
        ))}
    </Table>
  );
}
