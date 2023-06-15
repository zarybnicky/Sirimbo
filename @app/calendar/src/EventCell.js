import PropTypes from 'prop-types'
import React from 'react'
import clsx from 'clsx'
import localizer from './localizer'
import EventWrapper from './addons/dragAndDrop/EventWrapper';

class EventCell extends React.Component {
  render() {
    let Event = null;
    let {
      style,
      className,
      event,
      selected,
      isAllDay,
      onSelect,
      onDoubleClick,
      onKeyPress,
      continuesPrior,
      continuesAfter,
      slotStart,
      slotEnd,
      resizable: _,
      ...props
    } = this.props

    let { start, end, title, allDay } = event;

    let showAsAllDay =
      isAllDay ||
      allDay ||
      localizer.diff(start, localizer.ceil(end, 'day'), 'day') > 1

    return (
      <EventWrapper {...this.props} type="date">
        <div
          {...props}
          tabIndex={0}
          style={style}
          className={clsx('rbc-event', className, {
            'rbc-selected': selected,
            'rbc-event-allday': showAsAllDay,
            'rbc-event-continues-prior': continuesPrior,
            'rbc-event-continues-after': continuesAfter,
            'rbc-draggable': event.isDraggable !== false,
            'rbc-nondraggable': event.isDraggable === false,
          })}
          onClick={(e) => onSelect && onSelect(event, e)}
          onDoubleClick={(e) => onDoubleClick && onDoubleClick(event, e)}
          onKeyPress={(e) => onKeyPress && onKeyPress(event, e)}
        >
          <div className="rbc-event-content" title={title}>
            {Event ? (
              <Event
                event={event}
                continuesPrior={continuesPrior}
                continuesAfter={continuesAfter}
                title={title}
                isAllDay={allDay}
                slotStart={slotStart}
                slotEnd={slotEnd}
              />
            ) : (
              title
            )}
          </div>
        </div>
      </EventWrapper>
    )
  }
}

EventCell.propTypes = {
  event: PropTypes.object.isRequired,
  slotStart: PropTypes.instanceOf(Date),
  slotEnd: PropTypes.instanceOf(Date),

  resizable: PropTypes.bool,
  selected: PropTypes.bool,
  isAllDay: PropTypes.bool,
  continuesPrior: PropTypes.bool,
  continuesAfter: PropTypes.bool,

  onSelect: PropTypes.func,
  onDoubleClick: PropTypes.func,
  onKeyPress: PropTypes.func,
}

export default EventCell
