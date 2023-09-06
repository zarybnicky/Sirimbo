import classnames from 'classnames';
import { shortTimeIntl } from './localizer';
import React from 'react';
import { TimeSlotMetrics } from './TimeSlotMetrics';
import { CalendarEvent } from './types';
import { DnDContext, DragDirection } from './DnDContext';
import { Popover, PopoverContent, PopoverTrigger } from '@app/ui/popover';
import { EventSummary } from '@app/ui/EventSummary';

function stringifyPercent(v: string | number) {
  return typeof v === 'string' ? v : v + '%';
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
  resourceId?: number;
};

function TimeGridEvent({
  style,
  className,
  event,
  isBackgroundEvent,
  slotMetrics,
  resourceId,
}: TimeGridEventProps) {
  const draggable = React.useContext(DnDContext);
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
      if (!!(e as React.MouseEvent).button) {
        return;
      }
      const resizeDirection = (e.target as HTMLElement).dataset.resize;
      if (isResizable && resizeDirection) {
        draggable.onBeginAction(event, 'resize', resizeDirection as DragDirection);
      } else if (isDraggable) {
        event.sourceResource = resourceId;
        draggable.onBeginAction(event, 'move');
      }
    },
    [draggable, event, isDraggable, isResizable, resourceId],
  );

  const label = React.useMemo(() => {
    if (startsBeforeDay && startsAfterDay) {
      return 'Celý den';
    }
    if (startsBeforeDay) {
      return ` – ${shortTimeIntl.format(event.end)}`;
    }
    if (startsAfterDay) {
      return `${shortTimeIntl.format(event.start)} – `;
    }
    return shortTimeIntl.formatRange(event.start, event.end);
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
        title={[label, event.title].filter(Boolean).join(': ')}
        className={classnames(className, {
          'rbc-event group transition-opacity': true,
          'rbc-resizable': isResizable,
          'empty-event': event.event?.eventRegistrations.totalCount  === 0,
          'is-group': event.event?.type === 'GROUP',
          // TODO: 'rbc-selected': selected,
          'opacity-75': isBackgroundEvent,
          'rbc-drag-preview': event.__isPreview,
          'rounded-t-none': continuesPrior,
          'rounded-b-none': continuesAfter,
          'rbc-dragged-event':
          draggable.stateRef.current.interacting &&
          draggable.stateRef.current.event === event,
        })}
      >
        {!continuesPrior && isResizable && (
          <div
            className="absolute top-0 opacity-0 group-hover:opacity-100 cursor-n-resize w-3 left-1/2 mx-auto border-t-[6px] border-double"
            data-resize="UP"
          />
        )}

        <div className="rbc-event-content">{event.title || '-'}</div>
        <div className="rbc-event-label">{label}</div>

        {!continuesPrior && isResizable && (
          <div
            className="absolute bottom-0 opacity-0 group-hover:opacity-100 cursor-s-resize w-3 left-1/2 mx-auto border-t-[6px] border-double"
            data-resize="DOWN"
          />
        )}
      </PopoverTrigger>

      <PopoverContent>
        <EventSummary instance={event} />
      </PopoverContent>
    </Popover>
  );
}

export default TimeGridEvent;
