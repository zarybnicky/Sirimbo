import React from 'react'
import { Navigate, ViewClass } from '../types'
import TimeGrid from '../TimeGrid'
import { startOf, endOf, add, range, startOfWeek } from '../localizer'

const Week: ViewClass = (props) => <TimeGrid {...props} />

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

export default Week