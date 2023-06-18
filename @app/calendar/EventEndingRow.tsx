import range from 'lodash/range'
import React, { useContext } from 'react'
import { eventLevels, Segment } from './common'
import type { DateSlotMetrics } from './DateSlotMetrics'
import EventCell from './EventCell'
import { NavigationContext } from './NavigationContext'
import { View } from './types'

let isSegmentInSlot = (seg: Segment, slot: number) => seg.left <= slot && seg.right >= slot
let eventsInSlot = (segments: Segment[], s: number) => segments.filter((seg) => isSegmentInSlot(seg, s)).length

const EventEndingRow: React.FC<{
  className?: string;
  segments: Segment[];
  slotMetrics: DateSlotMetrics,
  resourceId?: number;
}> = ({
  className,
  segments = [],
  slotMetrics,
  ...props
}) => {
  const { onDrillDown } = useContext(NavigationContext);
  const { slots } = slotMetrics;
  let rowSegments = eventLevels(segments).levels[0]!

  let current = 1,
      lastEnd = 1,
      row: JSX.Element[] = []

  while (current <= slots) {
    let key = '_lvl_' + current

    let { event, left, right, span } = rowSegments.filter((seg) => isSegmentInSlot(seg, current))[0]! || {}

    if (!event) {
      current++
      continue
    }

    let gap = Math.max(0, left - lastEnd)

    if (range(left, left + span).every((s: number) => eventsInSlot(segments, s) === 1)) {
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
            {...props}
          />
        </div>
      )
      lastEnd = current = right + 1
    } else {
      if (gap) {
        row.push(
          <div key={`${key}_gap`} className="rbc-row-segment" style={{ flexBasis: `${(Math.abs(gap) / slots) * 100}%` }} />
        );
      }
      let count = eventsInSlot(segments, current)
      row.push(
        <div key={key} className="rbc-row-segment" style={{ flexBasis: `${(1 / slots) * 100}%` }}>
          {!count ? null : (
            <button
              type="button"
              key={'sm_' + current}
              className="rbc-button-link rbc-show-more"
              onClick={(e) => {
                e.preventDefault()
                e.stopPropagation()
                onDrillDown(slotMetrics.getDateForSlot(current), View.DAY)
              }}
            >
              {`+${count} dalších`}
            </button>
          )}
        </div>
      )
      lastEnd = current = current + 1
    }
  }

  return <div className="rbc-row">{row}</div>
}

export default EventEndingRow
