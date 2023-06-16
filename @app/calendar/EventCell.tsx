import React from 'react'
import clsx from 'clsx'
import { ceil, diff } from './localizer'
import EventWrapper from './EventWrapper';
import { Event } from './types';
import { SelectionContext } from 'SelectContext';

type EventCellProps = {
  style?: React.CSSProperties;
  className?: string;
  event: Event;
  isAllDay?: boolean;
  continuesPrior: boolean;
  continuesAfter: boolean;
  resourceId?: number;
}

const EventCell = (props: EventCellProps) => {
  const {
    style,
    className,
    event,
    isAllDay,
    continuesPrior,
    continuesAfter,
  } = props
  const { onSelectEvent } = React.useContext(SelectionContext);

  return (
    <EventWrapper {...props} type="date">
      <div
        tabIndex={0}
        style={style}
        className={clsx('rbc-event', className, {
          // TODO: 'rbc-selected': selected,
          'rbc-event-allday': isAllDay || event.allDay || diff(event.start, ceil(event.end, 'day'), 'day') > 1,
          'rbc-event-continues-prior': continuesPrior,
          'rbc-event-continues-after': continuesAfter,
          'rbc-draggable': event.isDraggable !== false,
          'rbc-nondraggable': event.isDraggable === false,
        })}
        onClick={() => onSelectEvent(event)}
      >
        <div className="rbc-event-content" title={event.title?.toString()}>
          {event.title}
        </div>
      </div>
    </EventWrapper>
  )
}

export default EventCell
