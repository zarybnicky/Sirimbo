import React from 'react'
import clsx from 'clsx'
import { ceil, diff } from './localizer'
import EventWrapper from './EventWrapper';
import { Event } from './utils/constants';

type EventCellProps = {
  style?: React.CSSProperties;
  className?: string;
  event: Event;
  selected?: Event;
  isAllDay?: boolean;
  continuesPrior: boolean;
  continuesAfter: boolean;
  onSelectEvent: (event: Event) => void;
  resourceId?: number;
}

const EventCell = (props: EventCellProps) => {
  const {
    style,
    className,
    event,
    selected,
    isAllDay,
    continuesPrior,
    continuesAfter,
    onSelectEvent,
  } = props

  return (
    <EventWrapper {...props} type="date">
      <div
        tabIndex={0}
        style={style}
        className={clsx('rbc-event', className, {
          'rbc-selected': selected,
          'rbc-event-allday': isAllDay || event.allDay || diff(event.start, ceil(event.end, 'day'), 'day') > 1,
          'rbc-event-continues-prior': continuesPrior,
          'rbc-event-continues-after': continuesAfter,
          'rbc-draggable': event.isDraggable !== false,
          'rbc-nondraggable': event.isDraggable === false,
        })}
        onClick={(e) => onSelectEvent(event)}
      >
        <div className="rbc-event-content" title={event.title?.toString()}>
          {event.title}
        </div>
      </div>
    </EventWrapper>
  )
}

export default EventCell
