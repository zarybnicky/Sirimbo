import chunk from 'lodash.chunk'
import React from 'react'
import DateContentRow from '../DateContentRow'
import { format, inEventRange, range, sortEvents } from '../localizer'
import type { ViewProps } from '../types'

function MonthView({ date, range: days, events }: ViewProps) {
  const weeks = chunk(days, 7);
  const containerRef = React.useRef<HTMLDivElement>(null);

  return (
    <div className="rbc-month-view overscroll-contain" role="table" aria-label="Month View" ref={containerRef}>
      <div className="rbc-row flex" role="row">
        {range(weeks[0]?.[0]!, weeks[0]?.[weeks[0]?.length - 1]!, 'day').map((day, idx) => (
          <div key={`header_${idx}`} className="rbc-header">
            <span role="columnheader" aria-sort="none">
              {format(day, 'cccc')}
            </span>
          </div>
        ))}
      </div>

      {weeks.map((week, weekIdx) => (
        <DateContentRow
          className="rbc-month-row"
          key={weekIdx}
          range={week}
          date={date}
          containerRef={containerRef}
          events={
            events
            .filter(e => e.event?.type !== 'LESSON')
            .filter((e) => inEventRange(e, {start: week[0]!, end: week.at(-1)!}))
            .sort(sortEvents)
          }
        />
      ))}
    </div>
  );
}

export default MonthView
