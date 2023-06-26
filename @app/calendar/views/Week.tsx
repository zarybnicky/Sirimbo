import React from 'react'
import { Navigate, ViewClass } from '../types'
import TimeGrid from '../TimeGrid'
import { range } from '../localizer'
import { startOf, endOf, add } from 'date-arithmetic';

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

Week.range = (date) => range(startOf(date, 'week', 1), endOf(date, 'week', 1))

export default Week
