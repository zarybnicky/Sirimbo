import React from 'react';

type Constraint = { maxAge?: number; minAge?: number };

type Category = {
  name: string;
  older: Constraint;
  younger: Constraint;
};

export function AgeCategoryTable() {
  const year = new Date().getFullYear();

  const categories: Category[] = [
    { name: 'Do 8 let', older: { maxAge: 8 }, younger: { maxAge: 8 } },
    { name: 'Děti I', older: { maxAge: 10 }, younger: { maxAge: 10 } },
    { name: 'Děti II', older: { maxAge: 12, minAge: 10 }, younger: { maxAge: 12 } },
    { name: 'Junioři I', older: { maxAge: 14, minAge: 12 }, younger: { maxAge: 14 } },
    { name: 'Junioři II', older: { maxAge: 16, minAge: 14 }, younger: { maxAge: 16 } },
    { name: 'Mládež', older: { maxAge: 19, minAge: 16 }, younger: { maxAge: 19 } },
    { name: 'Do 21 let', older: { maxAge: 21, minAge: 16 }, younger: { maxAge: 21 } },
    { name: 'Dospělí', older: { minAge: 19 }, younger: { maxAge: 35 } },
    { name: 'Senioři I', older: { minAge: 35 }, younger: { minAge: 30 } },
    { name: 'Senioři II', older: { minAge: 45 }, younger: { minAge: 40 } },
    { name: 'Senioři III', older: { minAge: 55 }, younger: { minAge: 50 } },
    { name: 'Senioři IV', older: { minAge: 65 }, younger: { minAge: 60 } },
  ];

  const formatAge = ({ minAge, maxAge }: Constraint): string => {
    if (minAge !== undefined && maxAge !== undefined) return `${minAge}–${maxAge - 1}`;
    if (maxAge !== undefined) return `<${maxAge}`;
    if (minAge !== undefined) return `${minAge}+`;
    return '—';
  };

  const formatYears = (year: number, { minAge, maxAge }: Constraint): string => {
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
