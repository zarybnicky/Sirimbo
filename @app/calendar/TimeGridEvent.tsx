import clsx from 'clsx'
import React from 'react'
import EventWrapper from './EventWrapper';
import { Event } from './utils/constants';

function stringifyPercent(v: string | number) {
  return typeof v === 'string' ? v : v + '%'
}

function TimeGridEvent(props: {
  style: { top: number|string, width: number|string, height: number|string, xOffset: number};
  className?: string;
  event: Event;
  isBackgroundEvent?: boolean;
  selected?: Event;
  label?: string;
  continuesPrior?: boolean;
  continuesAfter?: boolean;
  onClick?: React.MouseEventHandler<HTMLDivElement>;
}) {
  const {
    style,
    className,
    event,
    selected,
    label,
    continuesPrior,
    continuesAfter,
    onClick,
    isBackgroundEvent,
  } = props
  return (
    <EventWrapper type="time" {...props}>
      <div
        onClick={onClick}
        style={{
          top: stringifyPercent(style.top),
          width: isBackgroundEvent ? `calc(${style.width} + 10px)` : stringifyPercent(style.width),
          height: stringifyPercent(style.height),
          left: stringifyPercent(Math.max(0, style.xOffset)),
        }}
        title={[label, event.title].filter(Boolean).join(': ')}
        className={clsx(
          isBackgroundEvent ? 'rbc-background-event' : 'rbc-event',
          className,
          {
            'rbc-selected': selected,
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
