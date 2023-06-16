import React from 'react'
import { Navigate, ViewClass } from './utils/constants'
import TimeGrid from './TimeGrid'
import { startOf, endOf, add, dayRangeHeaderFormat, range, startOfWeek } from './localizer'

const Week: ViewClass = (props) => (
  <TimeGrid
    {...props}
    range={Week.range(props.date)}
    min={props.min || startOf(new Date(), 'day')}
    max={props.max || endOf(new Date(), 'day')}
  />
)

Week.navigate = (date, action) => {
  switch (action) {
    case Navigate.PREVIOUS:
      return add(date, -1, 'week')

    case Navigate.NEXT:
      return add(date, 1, 'week')

    default:
      return date
  }
}

Week.range = (date) => {
  let start = startOf(date, 'week', startOfWeek)
  let end = endOf(date, 'week', startOfWeek)
  return range(start, end)
}

Week.title = (date) => {
  let [start, ...rest] = Week.range(date)
  return dayRangeHeaderFormat({ start, end: rest.pop() })
}
Week.name = "Týden";

export default Week
