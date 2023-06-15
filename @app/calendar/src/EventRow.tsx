import clsx from 'clsx'
import React from 'react'
import { isSelected } from './utils/selection'
import EventCell from './EventCell'

const EventRow: React.FC<{
  className?: string;
  segments: {
    event: Event;
    left: number;
    right: number;
    span: number;
  }[];
  slotMetrics: SlotMetrics,
  resizable?: boolean;
  selected: object,
  onSelect: () => void;
  onDoubleClick: () => void;
  onKeyPress: () => void;
}> = ({
  className,
  segments = [],
  selected = {},
  slotMetrics,
  ...props
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
          slotStart={slotMetrics.first}
          slotEnd={slotMetrics.last}
          selected={isSelected(event, selected)}
          {...props}
        />
      </div>
    )
    lastEnd = right + 1
  });

  return <div className={clsx(className, 'rbc-row')}>{row}</div>
}

export default EventRow
