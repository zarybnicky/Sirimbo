import chunk from 'lodash.chunk';
import React from 'react';
import DateContentRow from '../DateContentRow';
import { ceil, diff, format, inEventRange, range } from '../localizer';
import type { ViewProps } from '../types';
import { startOf } from 'date-arithmetic';

function MonthView({ date, range: days, events }: ViewProps) {
  const weeks = chunk(days, 7);
  const containerRef = React.useRef<HTMLDivElement>(null);

  return (
    <div
      className="rbc-month-view overscroll-contain"
      role="table"
      aria-label="Month View"
      ref={containerRef}
    >
      <div className="rbc-row flex" role="row">
        {range(weeks[0]?.[0]!, weeks[0]?.[weeks[0]?.length - 1]!, 'day').map(
          (day, idx) => (
            <div key={`header_${idx}`} className="rbc-header">
              <span role="columnheader" aria-sort="none">
                {format(day, 'cccc')}
              </span>
            </div>
          ),
        )}
      </div>

      {weeks.map((week, weekIdx) => (
        <DateContentRow
          className="rbc-month-row"
          key={weekIdx}
          range={week}
          date={date}
          containerRef={containerRef}
          events={events
            .filter((e) => e.instance.type !== 'LESSON')
            .filter((e) => inEventRange(e, { start: week[0]!, end: week.at(-1)! }))
            .toSorted(sortEvents)}
        />
      ))}
    </div>
  );
}

export default MonthView;

function sortEvents(a: { start: Date; end: Date }, b: { start: Date; end: Date }) {
  const startSort = +startOf(a.start, 'day') - +startOf(b.start, 'day');
  const durA = diff(a.start, ceil(a.end, 'day'), 'day');
  const durB = diff(b.start, ceil(b.end, 'day'), 'day');

  return (
    startSort || // sort by start Day first
    Math.max(durB, 1) - Math.max(durA, 1) || // events spanning multiple days go first
    +a.start - +b.start || // then sort by start time
    +a.end - +b.end // then sort by end time
  );
}
