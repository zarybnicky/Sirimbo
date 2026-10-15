import { type AgeRange, compareAgeRanges, formatAgeGroup } from '@/ui/csts';
import { AgeGroupRulesDocument } from '@/graphql/AgeGroups';
import { executeGraphql } from '@/lib/server/graphql';

const formatAge = ({ start, end }: AgeRange): string => {
  const minAge = start?.value;
  const maxAge = end?.value;
  if (minAge !== undefined && maxAge !== undefined) return `${minAge}–${maxAge - 1}`;
  if (maxAge !== undefined) return `<${maxAge}`;
  if (minAge !== undefined) return `${minAge}+`;
  return '—';
};

const formatYears = (year: number, { start, end }: AgeRange): string => {
  const from = end ? year - end.value + 1 : undefined;
  const to = start ? year - start.value : undefined;

  if (from !== undefined && to !== undefined) {
    if (from > to) return '—';
    const count = to - from + 1;
    if (count === 1) return `${from}`;
    if (count <= 5) {
      return Array.from({ length: count }, (_, i) => from + i).join(', ');
    }
    return `${from}–${to}`;
  }
  if (from !== undefined) return `${from} a mladší`;
  if (to !== undefined) return `${to} a starší`;
  return '—';
};

export async function AgeCategoryTable() {
  const year = new Date().getFullYear();
  const { ageGroupsList } = await executeGraphql(AgeGroupRulesDocument);
  const categories = (ageGroupsList ?? [])
    .flatMap(({ coupleRules, code, name }) =>
      coupleRules.map((rule) => ({ code, name, ...rule })),
    )
    .toSorted((a, b) => compareAgeRanges(a.olderAge, b.olderAge));

  return (
    <table className="grid grid-cols-[auto_min-content_1fr_min-content_1fr] gap-x-4 gap-y-1">
      <thead className="contents">
        <tr className="contents">
          <th className="text-left">Kategorie</th>
          <th className="col-span-2 text-left">Starší partner</th>
          <th className="col-span-2 text-left">Mladší partner</th>
        </tr>
      </thead>
      <tbody className="contents">
        {categories.map((c) => (
          <tr key={c.code} className="contents">
            <td className="self-center">{formatAgeGroup(c.code, c.name)}</td>
            <td className="self-center text-center px-0">
              <span className="bg-accent-3 py-1 px-2 rounded-md whitespace-nowrap">
                {formatAge(c.olderAge)}
              </span>
            </td>
            <td className="self-center px-0">{formatYears(year, c.olderAge)}</td>
            <td className="self-center text-center px-0">
              <span className="bg-accent-3 py-1 px-2 rounded-md whitespace-nowrap">
                {formatAge(c.youngerAge)}
              </span>
            </td>
            <td className="self-center px-0">{formatYears(year, c.youngerAge)}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}
