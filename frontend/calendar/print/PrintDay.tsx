import React from 'react';
import { startOf } from 'date-arithmetic';
import { format } from '@/calendar/localizer';
import { capitalize } from '@/ui/format';
import TimeGrid from '@/calendar/TimeGrid';
import type { CalendarEvent, Resource } from '@/calendar/types';

const emptyEvents: readonly CalendarEvent[] = [];

/**
 * Print-ready day: the calendar's own time-grid for a single day, rendered
 * static (no scroll) via print CSS. Reuses TimeGrid's proportional layout and
 * overlap packing verbatim.
 */
export function PrintDay({
  date,
  events,
  resources,
}: {
  date: Date;
  events: readonly CalendarEvent[];
  resources: readonly Resource[];
}) {
  const range = React.useMemo(
    () => ({ since: startOf(date, 'day'), until: startOf(date, 'day') }),
    [date],
  );

  return (
    <section className="print-day">
      <h2 className="mb-1 text-lg font-semibold break-after-avoid">
        {capitalize(format(date, 'EEEE d. MMMM yyyy'))}
      </h2>
      <TimeGrid
        range={range}
        events={events}
        backgroundEvents={emptyEvents}
        resources={resources}
        primary="resource"
      />
    </section>
  );
}
