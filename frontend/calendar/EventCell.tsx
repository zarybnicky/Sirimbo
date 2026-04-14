import React, { useCallback } from 'react';
import type { CalendarEvent, DragDirection, Resource } from './types';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { EventSummary } from '@/ui/EventSummary';
import { useAtomValue, useSetAtom } from 'jotai';
import { type DragSubject, dragSubjectAtom } from './state';
import { cn } from '@/lib/cn';
import { selectAtom } from 'jotai/utils';
import { formatDefaultEventName } from '@/ui/format';
import { isTruthy } from '@/lib/truthyFilter';
import { ConflictsInstanceBadge } from '@/calendar/ConflictsInstanceBadge';

type EventCellProps = {
  style?: React.CSSProperties;
  className?: string;
  event: CalendarEvent;
  continuesPrior: boolean;
  continuesAfter: boolean;
  resource?: Resource;
};

function EventCell({
  style,
  className,
  event,
  continuesPrior,
  continuesAfter,
  resource,
}: EventCellProps) {
  const { instance } = event;
  const isResizable = event.isResizable !== false;
  const isDraggable = event.isDraggable !== false;

  const setDragSubject = useSetAtom(dragSubjectAtom);
  const getCurrentEvent = useCallback(
    (v: DragSubject) => (v?.event?.instance.id === instance.id ? v : null),
    [instance.id],
  );
  const currentDragSubject = useAtomValue(selectAtom(dragSubjectAtom, getCurrentEvent));

  const onTouchOrMouse = React.useCallback(
    (e: React.TouchEvent | React.MouseEvent) => {
      if ((e as React.MouseEvent).button) {
        return;
      }
      const resizeDirection = (e.target as HTMLElement).dataset.resize;
      if (isResizable && resizeDirection) {
        setDragSubject({
          action: 'resize',
          event,
          direction: resizeDirection as DragDirection,
        });
      } else if (isDraggable) {
        setDragSubject({ action: 'move', event: { ...event, sourceResource: resource } });
      }
    },
    [setDragSubject, event, isDraggable, isResizable, resource],
  );

  return (
    <Popover modal>
      <PopoverTrigger asChild>
        <div
          style={style}
          onMouseDown={onTouchOrMouse}
          onTouchStart={onTouchOrMouse}
          className={cn(className, {
            'rbc-event group transition-opacity': true,
            'relative w-full h-full': isResizable,
            'rounded-l-none': continuesPrior,
            'rounded-r-none': continuesAfter,
            'cursor-grab': event.isDraggable !== false,
            'rbc-nondraggable': event.isDraggable === false,
            'rbc-drag-preview': event.__isPreview,
            'rbc-dragged-event': !!currentDragSubject && !event.__isPreview,
            'pl-3': event.event.eventTargetCohortsList.length > 0,
            relative: true,
          })}
        >
          <ConflictsInstanceBadge
            instanceId={event.instance.id}
            className="absolute right-1 top-1 text-accent-11 drop-shadow"
          />
          {event.event.eventTargetCohortsList.length > 0 && (
            <div className="absolute rounded-l-lg overflow-hidden border-r border-neutral-6 shadow-sm inset-y-0 left-0 flex flex-col">
              {event.event.eventTargetCohortsList
                .map((x) => x.cohort?.colorRgb)
                .filter(isTruthy)
                .map((color) => (
                  <div
                    key={color}
                    className="flex-1 w-2"
                    style={{ backgroundColor: color }}
                  />
                ))}
            </div>
          )}

          {!continuesPrior && isResizable && (
            <div
              className="absolute left-0 opacity-0 group-hover:opacity-100 cursor-w-resize h-3 top-2 mx-auto border-l-4 border-double"
              data-resize="LEFT"
            />
          )}

          <div
            className={`rbc-event-content${instance.isCancelled ? ' line-through' : ''}`}
          >
            {event.instance.name ?? formatDefaultEventName(event.event)}
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
        <EventSummary offsetButtons event={event.event} instance={instance} />
      </PopoverContent>
    </Popover>
  );
}

export default EventCell;
