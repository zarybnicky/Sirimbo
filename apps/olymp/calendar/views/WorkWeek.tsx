import React from 'react'
import Week from './Week'
import TimeGrid from '../TimeGrid'
import { ViewClass } from '../types';

const WorkWeek: ViewClass = (props) => <TimeGrid {...props} />

WorkWeek.range = (date: Date) => Week.range(date).filter((d) => [6, 0].indexOf(d.getDay()) === -1);
WorkWeek.navigate = Week.navigate

export default WorkWeek
