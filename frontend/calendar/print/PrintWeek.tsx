import React from 'react';
import { range } from '@/calendar/localizer';
import type { CalendarInstanceEvent, DateRange, Resource } from '@/calendar/types';
import { PrintDay } from './PrintDay';

/**
 * Print-ready week: for now, simply the daily grid repeated for each day in
 * the range, one day per printed page.
 */
export function PrintWeek({
  range: { since, until },
  events,
  resources,
}: {
  range: DateRange;
  events: readonly CalendarInstanceEvent[];
  resources: readonly Resource[];
}) {
  const days = React.useMemo(() => range(since, until, 'day'), [since, until]);

  return (
    <div className="flex flex-col gap-8">
      {days.map((day, i) => (
        <div key={+day} className={i > 0 ? 'print-break-before' : undefined}>
          <PrintDay date={day} events={events} resources={resources} />
        </div>
      ))}
    </div>
  );
}
