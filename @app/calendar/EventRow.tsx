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
  segments.forEach((segment, current) => {
    const key = '_lvl_' + current
    const gap = Math.max(0, segment.left - lastEnd)
    if (gap) {
      const width = `${(Math.abs(gap) / slots) * 100}%`;
      row.push(<div key={`${key}_gap`} className="rbc-row-segment" style={{ width }} />);
    }
    const width = `${(Math.abs(segment.span) / slots) * 100}%`;
    row.push(
      <div key={key} className="rbc-row-segment" style={{ width }}>
        <EventCell
          event={segment.event}
          continuesPrior={slotMetrics.continuesPrior(segment.event)}
          continuesAfter={slotMetrics.continuesAfter(segment.event)}
          resourceId={resourceId}
        />
      </div>
    )
    lastEnd = segment.right + 1
  });

  return <div className={clsx(className, 'rbc-row')}>{row}</div>
}

export default EventRow
