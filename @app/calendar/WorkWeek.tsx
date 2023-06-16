import React from 'react'
import Week from './Week'
import TimeGrid from './TimeGrid'
import { startOf, endOf, dayRangeHeaderFormat } from './localizer'
import { ViewClass } from './utils/constants';

const WorkWeek: ViewClass = (props) => (
  <TimeGrid
    {...props}
    range={WorkWeek.range(props.date)}
    min={props.min || startOf(new Date(), 'day')}
    max={props.max || endOf(new Date(), 'day')}
  />
)

WorkWeek.range = (date: Date) => Week.range(date).filter((d) => [6, 0].indexOf(d.getDay()) === -1);

WorkWeek.navigate = Week.navigate

WorkWeek.title = (date) => {
  let [start, ...rest] = WorkWeek.range(date)
  return dayRangeHeaderFormat({ start, end: rest.pop() })
}
WorkWeek.name = "Pracovní týden";

export default WorkWeek
