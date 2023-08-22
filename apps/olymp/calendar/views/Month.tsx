import classnames from 'classnames'
import { add, endOf, eq, neq, startOf } from 'date-arithmetic'
import chunk from 'lodash.chunk'
import React, { useContext } from 'react'
import DateContentRow from '../DateContentRow'
import { NavigationContext } from '../NavigationContext'
import { format, inEventRange, range, sortEvents, startOfWeek } from '../localizer'
import { Navigate, View, ViewClass } from '../types'

const MonthView: ViewClass = ({ date: currentDate, range: days, events }) => {
  const weeks = chunk(days, 7);
  const containerRef = React.useRef<HTMLDivElement>(null);
  const { onDrillDown } = useContext(NavigationContext)

  return (
    <div className="rbc-month-view" role="table" aria-label="Month View" ref={containerRef}>
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
          events={events.filter((e) => inEventRange(e, {start: week[0]!, end: week[week.length - 1]!})).sort(sortEvents)}
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
                  onDrillDown(date, View.DAY)
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

MonthView.range = (date) => range(firstVisibleDay(date), lastVisibleDay(date), 'day')

MonthView.navigate = (date, action) => {
  switch (action) {
    case Navigate.PREVIOUS:
      return add(date, -1, 'month')
    case Navigate.NEXT:
      return add(date, 1, 'month')
    default:
      return date
  }
}

const firstVisibleDay = (date: Date) => startOf(startOf(date, 'month'), 'week', startOfWeek)
const lastVisibleDay = (date: Date) => endOf(endOf(date, 'month'), 'week', startOfWeek);

export default MonthView
