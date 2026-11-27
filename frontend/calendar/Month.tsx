import { eventInRange } from './common';
import React from 'react';
import DateContentRow from './DateContentRow';
import { weekdayFormatter } from '@/ui/format';
import type { ViewProps } from './types';
import { eachDayOfInterval, startOfDay, differenceInCalendarDays, subMilliseconds } from 'date-fns';

function MonthView({ range: { since, until }, events }: ViewProps) {
  const today = new Date();
  const weeks = React.useMemo(() => {
    const days = eachDayOfInterval({ start: since, end: until });
    return Array.from({ length: Math.ceil(days.length / 7) }, (_, i) =>
      days.slice(i * 7, i * 7 + 7),
    );
  }, [since, until]);
  const weekEvents = React.useMemo(() => {
    const monthEvents = events.filter(
      (e) => e.kind !== 'event' || e.instance.type !== 'LESSON',
    );
    return weeks.map((week) => {
      const start = week[0];
      const end = week.at(-1);
      if (!start || !end) return [];
      return monthEvents
        .filter((event) => eventInRange(event, { start, end }))
        .toSorted(sortEvents);
    });
  }, [weeks, events]);
  const containerRef = React.useRef<HTMLDivElement>(null);

  return (
    <div
      className="relative flex flex-col flex-1 w-full select-none border border-neutral-6 overscroll-contain"
      ref={containerRef}
    >
      <div className="rbc-row flex">
        {(weeks[0] ?? []).map((day, idx) => (
          <div key={`header_${idx}`} className="rbc-header">
            <span>{weekdayFormatter.format(day)}</span>
          </div>
        ))}
      </div>

      {weeks.map((week, weekIdx) => (
        <DateContentRow
          key={weekIdx}
          range={week}
          date={today}
          containerRef={containerRef}
          events={weekEvents[weekIdx] ?? []}
        />
      ))}
    </div>
  );
}

export default React.memo(MonthView);

function sortEvents(a: { start: Date; end: Date }, b: { start: Date; end: Date }) {
  const durA = differenceInCalendarDays(subMilliseconds(a.end, 1), a.start) + 1;
  const durB = differenceInCalendarDays(subMilliseconds(b.end, 1), b.start) + 1;

  return (
    +startOfDay(a.start) - +startOfDay(b.start) || // sort by start Day first
    Math.max(durB, 1) - Math.max(durA, 1) || // events spanning multiple days go first
    +a.start - +b.start || // then sort by start time
    +a.end - +b.end // then sort by end time
  );
}
