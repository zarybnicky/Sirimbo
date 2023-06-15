import clsx from 'clsx'
import React from 'react'
import EventWrapper from './addons/dragAndDrop/EventWrapper';

function stringifyPercent(v) {
  return typeof v === 'string' ? v : v + '%'
}

function TimeGridEvent(props) {
  const {
    style,
    className,
    event,
    selected,
    label,
    continuesPrior,
    continuesAfter,
    onClick,
    onDoubleClick,
    isBackgroundEvent,
    onKeyPress,
  } = props
  let Event = null
  let { title } = event;
  let { height, top, width, xOffset } = style

  const eventStyle = isBackgroundEvent
    ? {
        top: stringifyPercent(top),
        height: stringifyPercent(height),
        // Adding 10px to take events container right margin into account
        width: `calc(${width} + 10px)`,
        left: stringifyPercent(Math.max(0, xOffset)),
      }
    : {
        top: stringifyPercent(top),
        width: stringifyPercent(width),
        height: stringifyPercent(height),
        left: stringifyPercent(xOffset),
      }

  return (
    <EventWrapper type="time" {...props}>
    <div
      onClick={onClick}
      onDoubleClick={onDoubleClick}
      style={eventStyle}
      onKeyPress={onKeyPress}
      title={(typeof label === 'string' ? label + ': ' : '') + title}
      className={clsx(
        isBackgroundEvent ? 'rbc-background-event' : 'rbc-event',
        className,
        {
          'rbc-selected': selected,
          'rbc-event-continues-earlier': continuesPrior,
          'rbc-event-continues-later': continuesAfter,
        }
      )}
    >
      <div className="rbc-event-label">
        {label}
      </div>
      <div className="rbc-event-content">
        {Event ? <Event event={event} title={title} /> : title}
      </div>
    </div>
    </EventWrapper>
  )
}

export default TimeGridEvent
