import clsx from 'clsx'
import { timeRangeEndFormat, timeRangeFormat, timeRangeStartFormat } from './localizer';
import React from 'react'
import { SelectionContext } from './SelectContext';
import { TimeSlotMetrics } from './TimeSlotMetrics';
import EventWrapper from './EventWrapper';
import { Event } from './types';

function stringifyPercent(v: string | number) {
  return typeof v === 'string' ? v : v + '%'
}

type TimeGridEventProps = {
  style: { top: number|string, width: number|string, height: number|string, xOffset: number};
  className?: string;
  event: Event;
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
  const startsAfterDay = slotMetrics.startsAfterDay(event.end)
  const startsBefore = slotMetrics.startsBefore(event.start)
  const startsAfter = slotMetrics.startsAfter(event.end)

  let label = ""
  if (startsBeforeDay && startsAfterDay) label = "Celý den";
  else if (startsBeforeDay) label = timeRangeEndFormat(event)
  else if (startsAfterDay) label = timeRangeStartFormat(event)
  else label = timeRangeFormat(event)

  const continuesPrior = startsBeforeDay || startsBefore;
  const continuesAfter = startsAfterDay || startsAfter;

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
        className={clsx(
          'rbc-event',
          className,
          {
            // TODO: 'rbc-selected': selected,
            'opacity-75': isBackgroundEvent,
            'rbc-event-continues-earlier': continuesPrior,
            'rbc-event-continues-later': continuesAfter,
          }
        )}
      >
        <div className="rbc-event-label">
          {label}
        </div>
        <div className="rbc-event-content">
          {event.title}
        </div>
      </div>
    </EventWrapper>
  );
}

export default TimeGridEvent
