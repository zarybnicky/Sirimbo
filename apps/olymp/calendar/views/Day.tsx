import React from 'react'
import { Navigate, ViewClass } from '../types'
import TimeGrid from '../TimeGrid'
import { add, startOf } from 'date-arithmetic'

const Day: ViewClass = (props) => <TimeGrid {...props} />

Day.range = (date) => [startOf(date, 'day')];

Day.navigate = (date, action) => {
  switch (action) {
    case Navigate.PREVIOUS:
      return add(date, -1, 'day')
    case Navigate.NEXT:
      return add(date, 1, 'day')
    default:
      return date
  }
}

export default Day
