import { Children, type ReactNode } from 'react';
import { cn } from '@/lib/cn';
import { getErrorMessage } from '@/lib/errors';
import { Spinner } from '@/ui/Spinner';
import { AlertCircle } from 'lucide-react';

type TableStateProps = { busy?: boolean; error?: Error | null; empty?: string };

export function Table({
  columns,
  caption,
  children,
  empty = 'Žádná data',
  className,
  busy,
  error,
}: {
  columns: (ReactNode | { label: ReactNode; colSpan: number })[];
  caption?: ReactNode;
  children: ReactNode;
  className?: string;
} & TableStateProps) {
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
          <TableStatus
            colSpan={headers.reduce((sum, header) => sum + header.colSpan, 0)}
            busy={busy}
            error={error}
            empty={rows.length === 0 ? empty : undefined}
          />
          {rows}
        </tbody>
      </table>
    </div>
  );
}

export function TableStatus({
  colSpan,
  busy,
  error,
  empty,
}: TableStateProps & { colSpan: number }) {
  const status = busy ? (
    <div role="status" className="not-prose flex items-center gap-2 py-2 text-neutral-11">
      <Spinner />
      Načítám…
    </div>
  ) : error ? (
    <div role="alert" className="not-prose flex items-start gap-2 py-2 text-danger-11">
      <AlertCircle className="mt-0.5 size-4 shrink-0" aria-hidden />
      <div>
        <p>Nepodařilo se načíst data.</p>
        <p className="text-xs wrap-anywhere">
          {getErrorMessage(error.message) ?? error.message}
        </p>
      </div>
    </div>
  ) : empty ? (
    <p className="not-prose text-neutral-11">{empty}</p>
  ) : null;

  if (!status) return null;
  return (
    <tr>
      <td colSpan={Math.max(1, colSpan)}>{status}</td>
    </tr>
  );
}
