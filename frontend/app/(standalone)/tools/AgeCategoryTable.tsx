import { cstsAgeCategories, type AgeConstraint } from '@/ui/csts';

const formatAge = ({ minAge, maxAge }: AgeConstraint): string => {
  if (minAge !== undefined && maxAge !== undefined) return `${minAge}–${maxAge - 1}`;
  if (maxAge !== undefined) return `<${maxAge}`;
  if (minAge !== undefined) return `${minAge}+`;
  return '—';
};

const formatYears = (year: number, { minAge, maxAge }: AgeConstraint): string => {
  const from = maxAge !== undefined ? year - maxAge + 1 : undefined;
  const to = minAge !== undefined ? year - minAge : undefined;

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

export function AgeCategoryTable() {
  const year = new Date().getFullYear();

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
        {cstsAgeCategories.map((c) => (
          <tr key={c.name} className="contents">
            <td className="self-center">{c.name}</td>
            <td className="self-center text-center px-0">
              <span className="bg-accent-3 py-1 px-2 rounded-md whitespace-nowrap">
                {formatAge(c.older)}
              </span>
            </td>
            <td className="self-center px-0">{formatYears(year, c.older)}</td>
            <td className="self-center text-center px-0">
              <span className="bg-accent-3 py-1 px-2 rounded-md whitespace-nowrap">
                {formatAge(c.younger)}
              </span>
            </td>
            <td className="self-center px-0">{formatYears(year, c.younger)}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}
