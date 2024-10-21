import range from 'lodash.range'
import React from 'react'
import { eventLevels, type Segment } from './common'
import type { DateSlotMetrics } from './DateSlotMetrics'
import EventCell from './EventCell'
import { useAtomValue } from 'jotai'
import { dragListenersAtom } from './state'
import type { Resource } from './types'

const isSegmentInSlot = (seg: Segment, slot: number) => seg.left <= slot && seg.right >= slot
const eventsInSlot = (segments: Segment[], s: number) => segments.filter((seg) => isSegmentInSlot(seg, s)).length

const EventEndingRow: React.FC<{
  segments: Segment[];
  slotMetrics: DateSlotMetrics,
  resource?: Resource;
}> = ({
  segments,
  slotMetrics,
  resource,
}) => {
  const { onDrillDown } = useAtomValue(dragListenersAtom);
  const { slots } = slotMetrics;
  const rowSegments = eventLevels(segments).levels[0]!
  const row: JSX.Element[] = [];

  let current = 1;
  let lastEnd = 1;

  while (current <= slots) {
    const segment = rowSegments.find((s) => isSegmentInSlot(s, current))
    if (!segment?.event) {
      current++
      continue
    }

    const key = '_lvl_' + current
    const gap = Math.max(0, segment.left - lastEnd)

    const exactlyOneEvent = range(segment.left, segment.left + segment.span).every((s) => eventsInSlot(segments, s) === 1)
    if (!exactlyOneEvent) {
      const closureCurrent = current;
      if (gap) {
        const flexBasis = `${(Math.abs(gap) / slots) * 100}%`;
        row.push(<div key={`${key}_gap`} className="rbc-row-segment" style={{ flexBasis }} />);
      }
      const count = eventsInSlot(segments, current)
      const flexBasis = `${(Math.abs(segment.span) / slots) * 100}%`;
      row.push(
        <div key={key} className="rbc-row-segment" style={{ flexBasis }}>
          {!count ? null : (
            <button
              type="button"
              key={'sm_' + current}
              className="rbc-button-link rbc-show-more"
              onClick={(e) => {
                e.preventDefault()
                e.stopPropagation()
                onDrillDown(slotMetrics.getDateForSlot(closureCurrent - 1))
              }}
            >
              {`+${count} dalších`}
            </button>
          )}
        </div>
      )
      lastEnd = current = current + 1
      continue
    }

    if (gap) {
      const flexBasis = `${(Math.abs(gap) / slots) * 100}%`;
      row.push(<div key={`${key}_gap`} className="rbc-row-segment" style={{ flexBasis }} />);
    }
    const flexBasis = `${(Math.abs(segment.span) / slots) * 100}%`;
    row.push(
      <div key={key} className="rbc-row-segment" style={{ flexBasis }}>
        <EventCell
          event={segment.event}
          continuesPrior={slotMetrics.continuesPrior(segment.event)}
          continuesAfter={slotMetrics.continuesAfter(segment.event)}
          resource={resource}
        />
      </div>
    )
    lastEnd = current = segment.right + 1
  }

  return <div className="rbc-row">{row}</div>
}

export default EventEndingRow
