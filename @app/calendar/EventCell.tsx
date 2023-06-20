import clsx from 'clsx';
import React from 'react';
import EventWrapper from './EventWrapper';
import { ceil, diff } from './localizer';
import { SelectionContext } from './SelectContext';
import { CalendarEvent } from './types';

type EventCellProps = {
  style?: React.CSSProperties;
  className?: string;
  event: CalendarEvent;
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
          'rbc-drag-preview': event.__isPreview,
          'rbc-nondraggable': event.isDraggable === false,
          'cursor-grab': event.isDraggable !== false,
        })}
        onClick={() => onSelectEvent(event)}
      >
        <div className="rbc-event-content" title={event.title?.toString()}>
          {event.title || '-'}
        </div>
      </div>
    </EventWrapper>
  )
}

export default EventCell
