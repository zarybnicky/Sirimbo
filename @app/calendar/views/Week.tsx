import React from 'react'
import { Navigate, ViewClass } from '../types'
import TimeGrid from '../TimeGrid'
import { startOf, endOf, add, dayRangeHeaderFormat, range, startOfWeek } from '../localizer'

const Week: ViewClass = (props) => (
  <TimeGrid {...props} range={Week.range(props.date)} />
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
  let start = startOf(date, 'week', startOfWeek)
  let end = endOf(date, 'week', startOfWeek)
  return dayRangeHeaderFormat({ start, end })
}
Week.name = "Týden";

export default Week
