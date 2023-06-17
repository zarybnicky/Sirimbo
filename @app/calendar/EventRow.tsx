import clsx from 'clsx'
import React from 'react'
import EventCell from './EventCell'
import { DateSlotMetrics } from './DateSlotMetrics'
import { Segment } from './common'

const EventRow: React.FC<{
  className?: string;
  segments: Segment[];
  slotMetrics: DateSlotMetrics;
  resourceId?: number;
}> = ({
  className,
  segments = [],
  slotMetrics,
  resourceId,
}) => {
  const { slots } = slotMetrics;
  let lastEnd = 1
  const row: JSX.Element[] = []
  segments.forEach(({ event, left, right, span }, li) => {
    let key = '_lvl_' + li
    let gap = left - lastEnd
    if (gap) {
      row.push(
        <div key={`${key}_gap`} className="rbc-row-segment" style={{ flexBasis: `${(Math.abs(gap) / slots) * 100}%` }} />
      );
    }
    row.push(
      <div key={key} className="rbc-row-segment" style={{ flexBasis: `${(Math.abs(span) / slots) * 100}%` }}>
        <EventCell
          event={event}
          continuesPrior={slotMetrics.continuesPrior(event)}
          continuesAfter={slotMetrics.continuesAfter(event)}
          resourceId={resourceId}
        />
      </div>
    )
    lastEnd = right + 1
  });

  return <div className={clsx(className, 'rbc-row')}>{row}</div>
}

export default EventRow
