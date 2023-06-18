import React, { useContext } from 'react'
import clsx from 'clsx'
import chunk from 'lodash/chunk'
import { View, Navigate } from '../types'
import DateContentRow from '../DateContentRow'
import { eq, neq, sortEvents, add, firstVisibleDay, format, lastVisibleDay, range, visibleDays, inEventRange } from '../localizer'
import { NavigationContext } from '../NavigationContext'
import { ViewClass } from '../types'

const MonthView: ViewClass = ({ date: currentDate, events }) => {
  let weeks = chunk(visibleDays(currentDate), 7);
  const { onDrillDown } = useContext(NavigationContext)

  return (
    <div className="rbc-month-view" role="table" aria-label="Month View">
      <div className="rbc-row rbc-month-header" role="row">
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
          key={weekIdx}
          className="rbc-month-row"
          date={currentDate}
          range={week}
          measureRows
          events={events.filter((e) => inEventRange(e, {start: week[0]!, end: week[week.length - 1]!})).sort(sortEvents)}
          renderHeader={({ date, className, ...props }) => (
            <div
              {...props}
              className={clsx({
                [className ?? '']: true,
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

export default MonthView
