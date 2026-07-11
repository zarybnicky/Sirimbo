import React from 'react';
import { cn } from '@/lib/cn';
import { format, shortTimeIntl } from '@/calendar/localizer';
import { capitalize } from '@/ui/format';
import type { CalendarInstanceEvent } from '@/calendar/types';
import {
  buildDayGrid,
  cellKind,
  cohortColor,
  eventLabel,
  eventsOnDay,
  type DayCell,
} from './model';

/**
 * Print-ready daily schedule: trainers across the top, time flowing down,
 * group lessons as colored blocks, booked lessons showing the client, and
 * offered-but-unbooked slots hatched as "free".
 */
export function PrintDay({
  date,
  events,
}: {
  date: Date;
  events: readonly CalendarInstanceEvent[];
}) {
  const grid = React.useMemo(() => buildDayGrid(eventsOnDay(events, date)), [events, date]);

  return (
    <section className="print-day break-inside-avoid">
      <h2 className="mb-1 text-lg font-semibold">
        {capitalize(format(date, 'EEEE d. MMMM yyyy'))}
      </h2>

      {grid.isEmpty ? (
        <p className="text-sm text-neutral-11">Žádné tréninky</p>
      ) : (
        <table className="print-grid w-full table-fixed border-collapse text-[11px]">
          <thead>
            <tr>
              <th className="w-14 border border-neutral-8 bg-neutral-3 p-1 text-left font-semibold">
                Čas
              </th>
              {grid.columns.map((col) => (
                <th
                  key={col.id}
                  className="border border-neutral-8 bg-neutral-3 p-1 text-center font-semibold"
                >
                  {col.name}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {grid.rows.map((row, ri) => (
              <tr key={+row.start} className="break-inside-avoid">
                <td className="border border-neutral-8 bg-neutral-2 p-1 text-right align-top tabular-nums">
                  {shortTimeIntl.format(row.start)}
                </td>
                {grid.columns.map((col, ci) => (
                  <Cell key={col.id} cell={grid.cells[ci]![ri]!} />
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
    return <td className="border border-neutral-8 p-1" />;
  }

  const { event, rowSpan } = cell;
  const kind = cellKind(event);
  const color = kind === 'group' ? cohortColor(event) : undefined;
  const label = eventLabel(event);
  const cancelled = event.instance.isCancelled;

  return (
    <td
      rowSpan={rowSpan}
      style={color ? { backgroundColor: color } : undefined}
      className={cn(
        'border border-neutral-8 p-1 align-top text-center leading-tight',
        kind === 'group' && !color && 'bg-neutral-4 font-medium',
        kind === 'group' && 'font-medium',
        kind === 'free' && 'print-hatch text-neutral-11',
        kind === 'booked' && 'bg-neutral-0',
      )}
    >
      <span className={cn('break-words', cancelled && 'line-through')}>{label}</span>
    </td>
  );
}
