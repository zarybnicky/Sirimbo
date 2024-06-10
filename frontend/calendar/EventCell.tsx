import React, { useCallback } from 'react';
import { CalendarEvent, DragDirection, Resource } from './types';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { EventSummary } from '@/ui/EventSummary';
import { useAtom, useSetAtom } from 'jotai';
import { DragSubject, dragSubjectAtom } from './state';
import { cn } from '@/ui/cn';
import { selectAtom } from 'jotai/utils';
import { formatDefaultEventName } from '@/ui/format';

type EventCellProps = {
  style?: React.CSSProperties;
  className?: string;
  event: CalendarEvent;
  continuesPrior: boolean;
  continuesAfter: boolean;
  resource?: Resource;
}

const EventCell = ({
  style,
  className,
  event,
  continuesPrior,
  continuesAfter,
  resource,
}: EventCellProps) => {
  const isResizable = event.isResizable !== false;
  const isDraggable = event.isDraggable !== false;

  const setDragSubject = useSetAtom(dragSubjectAtom);
  const getCurrentEvent = useCallback((v: DragSubject) => v?.event === event ? v : null, [event]);
  const [currentDragSubject] = useAtom(selectAtom(dragSubjectAtom, getCurrentEvent));

  const onTouchOrMouse = React.useCallback((e: React.TouchEvent | React.MouseEvent) => {
    if ((e as React.MouseEvent).button) {
      return;
    }
    const resizeDirection = (e.target as HTMLElement).dataset.resize
    if (isResizable && resizeDirection) {
      setDragSubject({ action: 'resize', event, direction: resizeDirection as DragDirection });
    } else if (isDraggable) {
      event.sourceResource = resource;
      setDragSubject({ action: 'move', event });
    }
  }, [setDragSubject, event, isDraggable, isResizable, resource]);

  return (
    <Popover>
      <PopoverTrigger asChild>
        <div
          tabIndex={0}
          style={style}
          onMouseDown={onTouchOrMouse}
          onTouchStart={onTouchOrMouse}
          className={cn(className, {
            'rbc-event group transition-opacity': true,
            // TODO: 'rbc-selected': selected,
            'rbc-resizable': isResizable,
            'rounded-l-none': continuesPrior,
            'rounded-r-none': continuesAfter,
            'cursor-grab': event.isDraggable !== false,
            'rbc-nondraggable': event.isDraggable === false,
            'rbc-drag-preview': event.__isPreview,
            'rbc-dragged-event': !!currentDragSubject,
          })}
        >
          {!continuesPrior && isResizable && (
            <div
              className="absolute left-0 opacity-0 group-hover:opacity-100 cursor-w-resize h-3 top-2 mx-auto border-l-4 border-double"
              data-resize="LEFT"
            />
          )}

          <div className={"rbc-event-content" + (event.isCancelled ? ' line-through' : '')}>
            {event.event ? formatDefaultEventName(event.event) : '-'}
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
        <EventSummary offsetButtons instance={event} />
      </PopoverContent>
    </Popover>
  )
}

export default EventCell
