import { Children, type ReactNode } from 'react';
import { cn } from '@/lib/cn';

export function Table({
  columns,
  caption,
  children,
  empty,
  className,
  busy,
}: {
  columns: (ReactNode | { label: ReactNode; colSpan: number })[];
  caption?: ReactNode;
  children: ReactNode;
  empty?: string;
  className?: string;
  busy?: boolean;
}) {
  const headers = columns
    .filter((c) => !!c)
    .map((c) =>
      typeof c === 'object' && c !== null && 'label' in c ? c : { label: c, colSpan: 1 },
    );
  const rows = Children.toArray(children);
  return (
    <div className={cn('overflow-x-auto', className)} aria-busy={busy}>
      <table className="w-full text-left text-sm tabular-nums [&>*>tr>*]:px-2 [&>*>tr>*]:py-1 [&>*>tr>:first-child]:pl-0 [&>*>tr>:last-child]:pr-0 [&>tbody>tr>th]:font-normal">
        {caption && <caption className="text-left font-bold mb-2">{caption}</caption>}
        <thead>
          <tr>
            {headers.map(({ label, colSpan }, i) => (
              <th key={i} scope={colSpan > 1 ? 'colgroup' : 'col'} colSpan={colSpan}>
                {label}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {rows.length > 0
            ? rows
            : empty && (
                <tr>
                  <td colSpan={headers.reduce((sum, header) => sum + header.colSpan, 0)}>
                    {empty}
                  </td>
                </tr>
              )}
        </tbody>
      </table>
    </div>
  );
}
