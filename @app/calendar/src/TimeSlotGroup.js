import PropTypes from 'prop-types'
import React from 'react'

export default class TimeSlotGroup extends React.Component {
  render() {
    const {renderSlot, group} = this.props

    return (
      <div className="rbc-timeslot-group">
        {group.map((value, idx) => (
          <div className='rbc-time-slot'>
            {renderSlot && renderSlot(value, idx)}
          </div>
        ))}
      </div>
    )
  }
}

TimeSlotGroup.propTypes = {
  renderSlot: PropTypes.func,
  group: PropTypes.array.isRequired,
  resource: PropTypes.any,
}
