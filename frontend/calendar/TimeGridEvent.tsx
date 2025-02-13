import { shortTimeIntl } from './localizer';
import React, { useCallback } from 'react';
import type { TimeSlotMetrics } from './TimeSlotMetrics';
import type { CalendarEvent, DragDirection, Resource } from './types';
import { Popover, PopoverContent, PopoverTrigger } from '@/ui/popover';
import { EventSummary } from '@/ui/EventSummary';
import { useAtom, useAtomValue, useSetAtom } from 'jotai';
import { type DragSubject, dragSubjectAtom, isDraggingAtom } from './state';
import { cn } from '@/ui/cn';
import { selectAtom } from 'jotai/utils';
import { formatDefaultEventName } from '@/ui/format';

function stringifyPercent(v: string | number) {
  return typeof v === 'string' ? v : `${v}%`;
}

type TimeGridEventProps = {
  style: {
    top: number | string;
    width: number | string;
    height: number | string;
    xOffset: number;
    left?: number;
  };
  className?: string;
  event: CalendarEvent;
  isBackgroundEvent?: boolean;
  slotMetrics: TimeSlotMetrics;
  resource?: Resource;
};

function TimeGridEvent({
  style,
  className,
  event,
  isBackgroundEvent,
  slotMetrics,
  resource,
}: TimeGridEventProps) {
  const isDragging = useAtomValue(isDraggingAtom);
  const setDragSubject = useSetAtom(dragSubjectAtom);
  const getCurrentEvent = useCallback((v: DragSubject) => v?.event === event ? v : null, [event]);
  const [currentDragSubject] = useAtom(selectAtom(dragSubjectAtom, getCurrentEvent));

  const isResizable = event.isResizable !== false;
  const isDraggable = event.isDraggable !== false;

  const startsBeforeDay = slotMetrics.startsBeforeDay(event.start);
  const startsBefore = slotMetrics.startsBefore(event.start);
  const continuesPrior = startsBeforeDay || startsBefore;

  const startsAfterDay = slotMetrics.startsAfterDay(event.end);
  const startsAfter = slotMetrics.startsAfter(event.end);
  const continuesAfter = startsAfterDay || startsAfter;

  const onTouchOrMouse = React.useCallback(
    (e: React.TouchEvent | React.MouseEvent) => {
      if ((e as React.MouseEvent).button) {
        return;
      }
      const resizeDirection = (e.target as HTMLElement).dataset.resize;
      if (isResizable && resizeDirection) {
        setDragSubject({ action: 'resize', event, direction: resizeDirection as DragDirection });
      } else if (isDraggable) {
        event.sourceResource = resource;
        setDragSubject({ action: 'move', event });
      }
    },
    [setDragSubject, event, isDraggable, isResizable, resource],
  );

  const title = event.event ? formatDefaultEventName(event.event) : '-';
  const label = React.useMemo(() => {
    let label = '';
    if (startsBeforeDay && startsAfterDay) {
      label += 'Celý den';
    } else if (startsBeforeDay) {
      label += ` – ${shortTimeIntl.format(event.end)}`;
    } else if (startsAfterDay) {
      label += `${shortTimeIntl.format(event.start)} – `;
    } else {
      label += shortTimeIntl.format(event.start);
    }
    for (const trainer of event.event?.eventTrainersList ?? []) {
      label += `, ${trainer.name}`;
    }
    return label;
  }, [event, startsAfterDay, startsBeforeDay]);

  return (
    <Popover>
      <PopoverTrigger
        onMouseDown={onTouchOrMouse}
        onTouchStart={onTouchOrMouse}
        style={{
          top: stringifyPercent(style.top),
          width: isBackgroundEvent
            ? `calc(${style.width} + 10px)`
            : stringifyPercent(style.width),
          height: stringifyPercent(style.height),
          left:
            typeof style.xOffset === 'string'
              ? style.xOffset
              : stringifyPercent(Math.max(0, style.xOffset)),
        }}
        title={[label, title].filter(Boolean).join(': ')}
        className={cn(className, {
          'rbc-event group transition-opacity': true,
          'rbc-resizable': isResizable,
          'empty-event': event.event?.eventRegistrations.totalCount === 0,
          'is-group': event.event?.type === 'GROUP',
          // TODO: 'rbc-selected': selected,
          'opacity-75': isBackgroundEvent,
          'rbc-drag-preview': event.__isPreview,
          'rounded-t-none': continuesPrior,
          'rounded-b-none': continuesAfter,
          'rbc-dragged-event': isDragging && currentDragSubject,
        })}
      >
        {!continuesPrior && isResizable && (
          <div
            className="absolute top-0 opacity-0 group-hover:opacity-100 cursor-n-resize w-3 left-1/2 mx-auto border-t-[6px] border-double"
            data-resize="UP"
          />
        )}

        <div className={`rbc-event-content${event.instance.isCancelled ? ' line-through' : ''}`}>
          {title}
        </div>
        <div className="rbc-event-label">{label}</div>

        {!continuesPrior && isResizable && (
          <div
            className="absolute bottom-0 opacity-0 group-hover:opacity-100 cursor-s-resize w-3 left-1/2 mx-auto border-t-[6px] border-double"
            data-resize="DOWN"
          />
        )}
      </PopoverTrigger>

      <PopoverContent>
        <EventSummary offsetButtons event={event.event} instance={event.instance} />
      </PopoverContent>
    </Popover>
  );
}

export default TimeGridEvent;
