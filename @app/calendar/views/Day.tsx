import React from 'react'
import { Navigate, ViewClass } from '../types'
import TimeGrid from '../TimeGrid'
import { add, startOf, endOf, format } from '../localizer'

const Day: ViewClass = (props) => (
  <TimeGrid
    {...props}
    range={Day.range(props.date)}
    min={props.min || startOf(new Date(), 'day')}
    max={props.max || endOf(new Date(), 'day')}
  />
)

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

Day.title = (date) => format(date, 'cccc MMM dd')
Day.name = "Den";

export default Day
