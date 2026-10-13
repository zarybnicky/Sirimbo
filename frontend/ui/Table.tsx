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
  columns: ReactNode[];
  caption?: ReactNode;
  children: ReactNode;
  empty?: string;
  className?: string;
  busy?: boolean;
}) {
  const headers = columns.filter((column) => column != null && column !== false);
  const rows = Children.toArray(children);
  return (
    <div className={cn('overflow-x-auto', className)} aria-busy={busy}>
      <table className="w-full text-left text-sm tabular-nums [&>*>tr>*]:px-2 [&>*>tr>*]:py-1 [&>tbody>tr>th]:font-normal">
        {caption && <caption className="text-left font-bold mb-2">{caption}</caption>}
        <thead>
          <tr>
            {headers.map((label, index) => (
              <th key={index} scope="col">
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
                  <td colSpan={headers.length}>{empty}</td>
                </tr>
              )}
        </tbody>
      </table>
    </div>
  );
}
