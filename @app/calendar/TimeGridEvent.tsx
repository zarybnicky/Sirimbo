import clsx from 'clsx'
import { format } from './localizer';
import React from 'react'
import { SelectionContext } from './SelectContext';
import { TimeSlotMetrics } from './TimeSlotMetrics';
import { CalendarEvent } from './types';
import { DnDContext } from './DnDContext';

function stringifyPercent(v: string | number) {
  return typeof v === 'string' ? v : v + '%'
}

type TimeGridEventProps = {
  style: { top: number|string, width: number|string, height: number|string, xOffset: number};
  className?: string;
  event: CalendarEvent;
  isBackgroundEvent?: boolean;
  slotMetrics: TimeSlotMetrics;
  resourceId?: number;
}

function TimeGridEvent({
  style,
  className,
  event,
  isBackgroundEvent,
  slotMetrics,
  resourceId,
}: TimeGridEventProps) {
  const draggable = React.useContext(DnDContext);
  const { onSelectEvent } = React.useContext(SelectionContext);
  const isResizable = event.isResizable !== false;
  const isDraggable = event.isDraggable !== false;

  const startsBeforeDay = slotMetrics.startsBeforeDay(event.start)
  const startsBefore = slotMetrics.startsBefore(event.start)
  const continuesPrior = startsBeforeDay || startsBefore;

  const startsAfterDay = slotMetrics.startsAfterDay(event.end)
  const startsAfter = slotMetrics.startsAfter(event.end)
  const continuesAfter = startsAfterDay || startsAfter;

  const label = React.useMemo(() => {
    if (startsBeforeDay && startsAfterDay) {
      return "Celý den";
    }
    if (startsBeforeDay) {
      return ` – ${format(event.end, 'p')}`
    }
    if (startsAfterDay) {
      return `${format(event.start, 'p')} – `
    }
    return `${format(event.start, 'p')} – ${format(event.end, 'p')}`;
  }, [continuesPrior, continuesAfter, event]);

  return (
    <div
      onClick={() => onSelectEvent(event)}
      onMouseDown={(e) => {
        if (e.button !== 0) return;
        if (!isDraggable) return;
        // hack: because of the way the anchors are arranged in the DOM, resize
        // anchor events will bubble up to the move anchor listener. Don't start
        // move operations when we're on a resize anchor.
        if (!(e.target as any).getAttribute('class')?.includes('rbc-resize-')) {
          event.sourceResource = resourceId;
          draggable.onBeginAction(event, 'move');
        }
      }}
      onTouchStart={(e) => {
        if (!isDraggable) return;
        if (!(e.target as any).getAttribute('class')?.includes('rbc-resize-')) {
          event.sourceResource = resourceId;
          draggable.onBeginAction(event, 'move');
        }
        }}
      style={{
        top: stringifyPercent(style.top),
        width: isBackgroundEvent ? `calc(${style.width} + 10px)` : stringifyPercent(style.width),
        height: stringifyPercent(style.height),
        left: stringifyPercent(Math.max(0, style.xOffset)),
      }}
      title={[label, event.title].filter(Boolean).join(': ')}
      className={clsx({
        'rbc-event': true,
        [className ?? '']: true,
        'rbc-resizable': isResizable,
        // TODO: 'rbc-selected': selected,
        'opacity-75': isBackgroundEvent,
        'rbc-drag-preview': event.__isPreview,
        'rbc-event-continues-earlier': continuesPrior,
        'rbc-event-continues-later': continuesAfter,
        'rbc-dragged-event': draggable.stateRef.current.interacting && draggable.stateRef.current.event === event,
      })}
    >
      {!continuesPrior && isResizable && (
        <div
          className="rbc-resize-ns-anchor"
          onMouseDown={(e) => {
            if (e.button !== 0) return;
            draggable.onBeginAction(event, 'resize', "UP");
          }}
        >
          <div className="rbc-resize-ns-icon" />
        </div>
      )}

      <div className="rbc-event-content">
        {event.title || '-'}
      </div>
      <div className="rbc-event-label">
        {label}
      </div>

      {!continuesPrior && isResizable && (
        <div
          className="rbc-resize-ns-anchor"
          onMouseDown={(e) => {
            if (e.button !== 0) return;
            draggable.onBeginAction(event, 'resize', "DOWN");
          }}
        >
          <div className="rbc-resize-ns-icon" />
        </div>
      )}
    </div>
  );
}

export default TimeGridEvent
