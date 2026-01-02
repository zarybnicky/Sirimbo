import React, { useCallback } from 'react';
import type { CalendarEvent, DragDirection, Resource } from './types';
import { shortTimeIntl } from './localizer';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { EventSummary } from '@/ui/EventSummary';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';
import { calendarConflictsFor, type DragSubject, dragSubjectAtom } from './state';
import { cn } from '@/ui/cn';
import { selectAtom } from 'jotai/utils';
import { formatDefaultEventName } from '@/ui/format';
import { isTruthy } from '@/ui/truthyFilter';
import { AlertTriangle } from 'lucide-react';

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
  const isResizable = event.isResizable !== false;
  const isDraggable = event.isDraggable !== false;

  const setDragSubject = useSetAtom(dragSubjectAtom);
  const getCurrentEvent = useCallback(
    (v: DragSubject) => (v?.event === event ? v : null),
    [event],
  );
  const [currentDragSubject] = useAtom(selectAtom(dragSubjectAtom, getCurrentEvent));

  const conflictsAtom = React.useMemo(
    () => calendarConflictsFor(event.instance.id),
    [event.instance.id],
  );
  const conflicts = useAtomValue(conflictsAtom);
  const conflictNames = React.useMemo(
    () =>
      conflicts
        .map((conflict) => conflict.personName ?? conflict.fallbackName)
        .join(', '),
    [conflicts],
  );
  const conflictSummary = React.useMemo(
    () =>
      conflicts
        .map((conflict) => {
          const person = conflict.personName ?? conflict.fallbackName;
          const range = shortTimeIntl.formatRange(
            new Date(conflict.otherSince),
            new Date(conflict.otherUntil),
          );
          return `${person}: ${conflict.otherEventName} (${range})`;
        })
        .join(' • '),
    [conflicts],
  );
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
        event.sourceResource = resource;
        setDragSubject({ action: 'move', event });
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
            // TODO: 'rbc-selected': selected,
            'rbc-resizable': isResizable,
            'rounded-l-none': continuesPrior,
            'rounded-r-none': continuesAfter,
            'cursor-grab': event.isDraggable !== false,
            'rbc-nondraggable': event.isDraggable === false,
            'rbc-drag-preview': event.__isPreview,
            'rbc-dragged-event': !!currentDragSubject,
            'pl-3': event.event.eventTargetCohortsList.length > 0,
            relative: true,
          })}
          title={conflictSummary ? `Kolize – ${conflictSummary}` : undefined}
        >
          {conflicts.length > 0 && (
            <>
              <div
                className="absolute right-1 top-1 text-accent-11 drop-shadow"
                aria-hidden
              >
                <AlertTriangle className="size-4" />
              </div>
              <span className="sr-only">Kolize: {conflictNames}</span>
            </>
          )}
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
            className={`rbc-event-content${event.instance.isCancelled ? ' line-through' : ''}`}
          >
            {formatDefaultEventName(event.event)}
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
        <EventSummary offsetButtons event={event.event} instance={event.instance} />
      </PopoverContent>
    </Popover>
  );
}

export default EventCell;
