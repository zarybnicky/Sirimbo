import classnames from 'classnames'
import { eq, neq } from 'date-arithmetic'
import chunk from 'lodash.chunk'
import React from 'react'
import DateContentRow from '../DateContentRow'
import { format, inEventRange, range, sortEvents } from '../localizer'
import { ViewClass } from '../types'
import { useAtomValue } from 'jotai'
import { dragListenersAtom } from '../state'

const MonthView: ViewClass = ({ date: currentDate, range: days, events }) => {
  const weeks = chunk(days, 7);
  const containerRef = React.useRef<HTMLDivElement>(null);
  const { onDrillDown } = useAtomValue(dragListenersAtom);

  return (
    <div className="rbc-month-view overscroll-contain" role="table" aria-label="Month View" ref={containerRef}>
      <div className="rbc-row flex" role="row">
        {range(weeks[0]![0]!, weeks[0]![weeks[0]!.length - 1]!, 'day').map((day, idx) => (
          <div key={'header_' + idx} className="rbc-header">
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
          measureRows
          containerRef={containerRef}
          events={
            events
            .filter(e => e.event?.type !== 'LESSON')
            .filter((e) => inEventRange(e, {start: week[0]!, end: week[week.length - 1]!}))
            .sort(sortEvents)
          }
          renderHeader={({ date, className, ...props }) => (
            <div
              {...props}
              className={classnames(className, {
                'rbc-off-range': neq(date, currentDate, 'month'),
                'rbc-current': eq(date, currentDate, 'day')
              })}
            >
              <button
                type="button"
                className="rbc-button-link"
                onClick={(e) => {
                  e.preventDefault()
                  onDrillDown(date)
                }}
              >
                {format(date, 'dd')}
              </button>
            </div>
          )}
        />
      ))}
    </div>
  );
}

export default MonthView
