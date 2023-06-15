import PropTypes from 'prop-types'
import React from 'react'
import { navigate } from './utils/constants'
import TimeGrid from './TimeGrid'
import { startOf, endOf, add, dayRangeHeaderFormat } from './localizer'

class Week extends React.Component {
  render() {
    let {
      date,
      min = startOf(new Date(), 'day'),
      max = endOf(new Date(), 'day'),
      scrollToTime = startOf(new Date(), 'day'),
      ...props
    } = this.props
    let range = Week.range(date, this.props)

    return (
      <TimeGrid
        {...props}
        range={range}
        eventOffset={15}
        min={min}
        max={max}
        scrollToTime={scrollToTime}
      />
    )
  }
}

Week.propTypes = {
  date: PropTypes.instanceOf(Date).isRequired,

  events: PropTypes.array.isRequired,
  backgroundEvents: PropTypes.array.isRequired,
  resources: PropTypes.array,

  step: PropTypes.number,
  timeslots: PropTypes.number,
  range: PropTypes.arrayOf(PropTypes.instanceOf(Date)),
  min: PropTypes.instanceOf(Date),
  max: PropTypes.instanceOf(Date),

  scrollToTime: PropTypes.instanceOf(Date),
  showMultiDayTimes: PropTypes.bool,

  resizable: PropTypes.bool,
  width: PropTypes.number,

  selected: PropTypes.object,

  onNavigate: PropTypes.func,
  onSelectSlot: PropTypes.func,
  onSelectEnd: PropTypes.func,
  onSelectStart: PropTypes.func,
  onSelectEvent: PropTypes.func,
  onDoubleClickEvent: PropTypes.func,
  onKeyPressEvent: PropTypes.func,
  onShowMore: PropTypes.func,
  onDrillDown: PropTypes.func,
  getDrilldownView: PropTypes.func.isRequired,

  dayLayoutAlgorithm: PropTypes.oneOf(['overlap', 'no-overlap']),

  doShowMoreDrillDown: PropTypes.bool,

  popup: PropTypes.bool,
  handleDragStart: PropTypes.func,
}

Week.defaultProps = TimeGrid.defaultProps

Week.navigate = (date, action) => {
  switch (action) {
    case navigate.PREVIOUS:
      return add(date, -1, 'week')

    case navigate.NEXT:
      return add(date, 1, 'week')

    default:
      return date
  }
}

Week.range = (date) => {
  let start = startOf(date, 'week', firstOfWeek)
  let end = endOf(date, 'week', firstOfWeek)
  return range(start, end)
}

Week.title = (date) => {
  let [start, ...rest] = Week.range(date)
  return dayRangeHeaderFormat({ start, end: rest.pop() })
}
Week.name = "Týden";

export default Week
