import clsx from 'clsx'
import { format } from './localizer';
import React from 'react'
import { SelectionContext } from './SelectContext';
import { TimeSlotMetrics } from './TimeSlotMetrics';
import EventWrapper from './EventWrapper';
import { CalendarEvent } from './types';

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
  const { onSelectEvent } = React.useContext(SelectionContext);

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
    <EventWrapper type="time" event={event} resourceId={resourceId}>
      <div
        onClick={() => onSelectEvent(event)}
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
          // TODO: 'rbc-selected': selected,
          'opacity-75': isBackgroundEvent,
          'rbc-drag-preview': event.__isPreview,
          'rbc-event-continues-earlier': continuesPrior,
          'rbc-event-continues-later': continuesAfter,
        })}
      >
        <div className="rbc-event-content">
          {event.title || '-'}
        </div>
        <div className="rbc-event-label">
          {label}
        </div>
      </div>
    </EventWrapper>
  );
}

export default TimeGridEvent
