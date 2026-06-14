import React from 'react';
import DateContentRow from './DateContentRow';
import { ceil, diff, format, inEventRange, range } from './localizer';
import type { ViewProps } from './types';
import { startOf } from 'date-arithmetic';

function MonthView({ range: { since, until }, events }: ViewProps) {
  const today = new Date();
  const weeks = React.useMemo(() => {
    const days = range(since, until, 'day');
    return Array.from({ length: Math.ceil(days.length / 7) }, (_, i) => days.slice(i * 7, i * 7 + 7));
  }, [since, until]);
  const weekEvents = React.useMemo(() => {
    const monthEvents = events.filter((e) => e.kind !== 'event' || e.instance.type !== 'LESSON');
    return weeks.map((week) => {
      const start = week[0];
      const end = week.at(-1);
      if (!start || !end) return [];
      return monthEvents
        .filter((event) => inEventRange(event, { start, end }))
        .toSorted(sortEvents);
    });
  }, [weeks, events]);
  const containerRef = React.useRef<HTMLDivElement>(null);

  return (
    <div
      className="relative flex flex-col flex-1 w-full select-none border border-neutral-6 overscroll-contain"
      role="table"
      ref={containerRef}
    >
      <div className="rbc-row flex" role="row">
        {(weeks[0] ?? []).map((day, idx) => (
          <div key={`header_${idx}`} className="rbc-header">
            <span role="columnheader" aria-sort="none">
              {format(day, 'cccc')}
            </span>
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

export default MonthView;

function sortEvents(a: { start: Date; end: Date }, b: { start: Date; end: Date }) {
  const durA = diff(a.start, ceil(a.end, 'day'), 'day');
  const durB = diff(b.start, ceil(b.end, 'day'), 'day');

  return (
    +startOf(a.start, 'day') - +startOf(b.start, 'day') || // sort by start Day first
    Math.max(durB, 1) - Math.max(durA, 1) || // events spanning multiple days go first
    +a.start - +b.start || // then sort by start time
    +a.end - +b.end // then sort by end time
  );
}
