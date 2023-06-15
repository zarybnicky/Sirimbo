import PropTypes from 'prop-types'
import React from 'react'
import Week from './Week'
import TimeGrid from './TimeGrid'
import localizer from './localizer'

function workWeekRange(date, options) {
  return Week.range(date, options).filter(
    (d) => [6, 0].indexOf(d.getDay()) === -1
  )
}

class WorkWeek extends React.Component {
  render() {
    let {
      date,
      min = localizer.startOf(new Date(), 'day'),
      max = localizer.endOf(new Date(), 'day'),
      scrollToTime = localizer.startOf(new Date(), 'day'),
      enableAutoScroll = true,
      ...props
    } = this.props
    let range = workWeekRange(date, this.props)
    return (
      <TimeGrid
        {...props}
        range={range}
        eventOffset={15}
        min={min}
        max={max}
        scrollToTime={scrollToTime}
        enableAutoScroll={enableAutoScroll}
      />
    )
  }
}

WorkWeek.propTypes = {
  date: PropTypes.instanceOf(Date).isRequired,
  min: PropTypes.instanceOf(Date),
  max: PropTypes.instanceOf(Date),
  scrollToTime: PropTypes.instanceOf(Date),
  enableAutoScroll: PropTypes.bool,
}

WorkWeek.defaultProps = TimeGrid.defaultProps

WorkWeek.range = workWeekRange

WorkWeek.navigate = Week.navigate

WorkWeek.title = (date) => {
  let [start, ...rest] = workWeekRange(date)
  return localizer.format({ start, end: rest.pop() }, 'dayRangeHeaderFormat')
}
WorkWeek.name = "Pracovní týden";

export default WorkWeek
