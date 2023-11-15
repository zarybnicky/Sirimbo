import classNames from 'classnames';
import React from 'react';
import { ceil, diff } from './localizer';
import { CalendarEvent, DragDirection } from './types';
import { Popover, PopoverContent, PopoverTrigger } from '@app/ui/popover';
import { EventSummary } from '@app/ui/EventSummary';
import { UpsertEventSmallButton } from '@/ui/event-form/UpsertEventForm';
import { DeleteInstanceButton } from '@/ui/DeleteEventButton';
import { useAtom } from 'jotai';
import { dragSubjectAtom } from './state';

type EventCellProps = {
  style?: React.CSSProperties;
  className?: string;
  event: CalendarEvent;
  isAllDay?: boolean;
  continuesPrior: boolean;
  continuesAfter: boolean;
  resourceId?: string;
}

const EventCell = ({
  style,
  className,
  event,
  isAllDay,
  continuesPrior,
  continuesAfter,
  resourceId,
}: EventCellProps) => {
  const isResizable = event.isResizable !== false;
  const isDraggable = event.isDraggable !== false;
  const [dragSubject, setDragSubject] = useAtom(dragSubjectAtom);

  const onTouchOrMouse = React.useCallback((e: React.TouchEvent | React.MouseEvent) => {
    if (!!(e as React.MouseEvent).button) {
      return;
    }
    const resizeDirection = (e.target as HTMLElement).dataset.resize
    if (isResizable && resizeDirection) {
      setDragSubject({ action: 'resize', event, direction: resizeDirection as DragDirection });
    } else if (isDraggable) {
      event.sourceResource = resourceId;
      setDragSubject({ action: 'move', event });
    }
  }, [setDragSubject, event, isDraggable, isResizable, resourceId]);

  return (
    <Popover>
      <PopoverTrigger asChild>
        <div
          tabIndex={0}
          style={style}
          onMouseDown={onTouchOrMouse}
          onTouchStart={onTouchOrMouse}
          className={classNames(className, {
            'rbc-event group transition-opacity': true,
            // TODO: 'rbc-selected': selected,
            'rbc-resizable': isResizable,
            'rbc-event-allday': isAllDay || event.allDay || diff(event.start, ceil(event.end, 'day'), 'day') > 1,
            'rounded-l-none': continuesPrior,
            'rounded-r-none': continuesAfter,
            'cursor-grab': event.isDraggable !== false,
            'rbc-nondraggable': event.isDraggable === false,
            'rbc-drag-preview': event.__isPreview,
            'rbc-dragged-event': dragSubject.event === event,
          })}
        >
          {!continuesPrior && isResizable && (
            <div
              className="absolute left-0 opacity-0 group-hover:opacity-100 cursor-w-resize h-3 top-2 mx-auto border-l-4 border-double"
              data-resize="LEFT"
            />
          )}

          <div className="rbc-event-content">
            {event.title || '-'}
          </div>

          {!continuesAfter && isResizable && (
            <div
              className="absolute right-0 opacity-0 group-hover:opacity-100 cursor-e-resize h-3 top-2 mx-auto border-l-4 border-double"
              data-resize="RIGHT"
            />
          )}
        </div>
      </PopoverTrigger>

      <PopoverContent>
        <EventSummary instance={event} />
        {event.event && <UpsertEventSmallButton className="absolute top-4 right-16" event={event.event} />}
        {event && <DeleteInstanceButton className="absolute top-4 right-10" instance={event} />}
      </PopoverContent>
    </Popover>
  )
}

export default EventCell
