import type { ReactNode } from 'react';
import { ArrowDown, ArrowUpDown } from 'lucide-react';
import { cn } from '@/lib/cn';
import { buttonCls } from '@/ui/style';

export function SortButton<Column extends string>({
  column,
  value,
  onChange,
  defaultDescending = false,
  children,
}: {
  column: Column;
  value: { column: Column; descending: boolean };
  onChange: (value: { column: Column; descending: boolean }) => void;
  defaultDescending?: boolean;
  children: ReactNode;
}) {
  return (
    <button
      type="button"
      aria-pressed={value.column === column}
      className={buttonCls({
        display: 'none',
        variant: 'none',
        size: 'none',
        className: 'inline-flex items-center gap-1 whitespace-nowrap text-neutral-12',
      })}
      onClick={() =>
        onChange({
          column,
          descending: value.column === column ? !value.descending : defaultDescending,
        })
      }
    >
      {children}
      {value.column === column ? (
        <>
          <ArrowDown
            className={cn('size-4', !value.descending && 'rotate-180')}
            aria-hidden
          />
          <span className="sr-only">
            {value.descending ? ', sestupně' : ', vzestupně'}
          </span>
        </>
      ) : (
        <ArrowUpDown className="size-4" aria-hidden />
      )}
    </button>
  );
}
