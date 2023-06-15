import PropTypes from 'prop-types'
import React from 'react'
import { navigate } from './utils/constants'
import TimeGrid from './TimeGrid'
import localizer from './localizer'

class Day extends React.Component {
  render() {
    let {
      date,
      min = localizer.startOf(new Date(), 'day'),
      max = localizer.endOf(new Date(), 'day'),
      scrollToTime = localizer.startOf(new Date(), 'day'),
      ...props
    } = this.props
    return (
      <TimeGrid
        {...props}
        range={Day.range(date)}
        eventOffset={10}
        min={min}
        max={max}
        scrollToTime={scrollToTime}
      />
    )
  }
}

Day.propTypes = {
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
  selectable: PropTypes.oneOf([true, false, 'ignoreEvents']),

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

Day.range = (date) => {
  return [localizer.startOf(date, 'day')]
}

Day.navigate = (date, action) => {
  switch (action) {
    case navigate.PREVIOUS:
      return localizer.add(date, -1, 'day')

    case navigate.NEXT:
      return localizer.add(date, 1, 'day')

    default:
      return date
  }
}

Day.title = (date) => localizer.format(date, 'cccc MMM dd')
Day.name = "Den";

export default Day
