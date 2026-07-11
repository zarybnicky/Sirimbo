import React from 'react';
import { cn } from '@/lib/cn';
import { format, shortTimeIntl } from '@/calendar/localizer';
import { capitalize } from '@/ui/format';
import type { CalendarInstanceEvent, Resource } from '@/calendar/types';
import { buildDayGrid, describeEvent, type DayCell } from './model';

/**
 * Print-ready daily schedule: resources (trainers) across the top, time
 * flowing down. Group lessons render as colored blocks, booked lessons show
 * the client, and offered-but-unbooked slots are hatched as "free".
 */
export function PrintDay({
  date,
  events,
  resources,
}: {
  date: Date;
  events: readonly CalendarInstanceEvent[];
  resources: readonly Resource[];
}) {
  const grid = React.useMemo(
    () => buildDayGrid(events, date, resources),
    [events, date, resources],
  );

  return (
    <section className="print-day break-inside-avoid">
      <h2 className="mb-1 text-lg font-semibold">
        {capitalize(format(date, 'EEEE d. MMMM yyyy'))}
      </h2>

      {grid.isEmpty ? (
        <p className="text-sm text-neutral-11">Žádné tréninky</p>
      ) : (
        <table className="w-full border-collapse text-[11px]">
          <thead>
            <tr>
              <th className="whitespace-nowrap border border-neutral-8 bg-neutral-3 px-1 py-0.5 text-left font-semibold">
                Čas
              </th>
              {grid.groupColumnCount > 0 && (
                <th
                  colSpan={grid.groupColumnCount}
                  className="border border-neutral-8 bg-neutral-3 px-1 py-0.5 text-center font-semibold"
                >
                  Společné
                </th>
              )}
              {grid.columns
                .filter((col) => col.kind === 'trainer')
                .map((col) => (
                  <th
                    key={col.key}
                    className="border border-neutral-8 bg-neutral-3 px-1 py-0.5 text-center font-semibold"
                  >
                    {col.title}
                  </th>
                ))}
            </tr>
          </thead>
          <tbody>
            {grid.rows.map((row, ri) => (
              <tr key={+row.start} className="break-inside-avoid">
                <td className="whitespace-nowrap border border-neutral-8 bg-neutral-2 px-1 py-0.5 text-right align-top tabular-nums">
                  {shortTimeIntl.format(row.start)}
                </td>
                {grid.columns.map((col, ci) => (
                  <Cell key={col.key} cell={grid.cells[ci]![ri]!} />
                ))}
              </tr>
            ))}
          </tbody>
        </table>
      )}
    </section>
  );
}

function Cell({ cell }: { cell: DayCell }) {
  if (cell.type === 'span') return null;
  if (cell.type === 'empty') {
    return <td className="border border-neutral-8" />;
  }

  const { isGroup, isBooked, label, color } = describeEvent(cell.event);
  const free = !isGroup && !isBooked;

  return (
    <td
      rowSpan={cell.rowSpan}
      style={isGroup && color ? { backgroundColor: color } : undefined}
      className={cn(
        'border border-neutral-8 px-1 py-0.5 text-center align-top leading-tight',
        isGroup && 'font-medium',
        isGroup && !color && 'bg-neutral-4',
        free && 'print-hatch text-neutral-11',
      )}
    >
      <span className={cn('break-words', cell.event.instance.isCancelled && 'line-through')}>
        {label}
      </span>
    </td>
  );
}
