import { type JSX } from 'react';
import { eventLevels, type Segment } from './common';
import type { DateSlotMetrics } from './DateSlotMetrics';
import EventCell from './EventCell';
import { useAtomValue } from 'jotai';
import { dragListenersAtom } from './state';

const isSegmentInSlot = (seg: Segment, slot: number) =>
  seg.left <= slot && seg.right >= slot;
const eventsInSlot = (segments: Segment[], s: number) =>
  segments.filter((seg) => isSegmentInSlot(seg, s)).length;

function EventEndingRow({
  segments,
  slotMetrics,
  onShowMore,
}: {
  segments: Segment[];
  slotMetrics: DateSlotMetrics;
  onShowMore?: (date: Date) => void;
}) {
  const { onDrillDown } = useAtomValue(dragListenersAtom);
  const { slots } = slotMetrics;
  const rowSegments = eventLevels(segments).levels[0]!;
  const row: JSX.Element[] = [];

  let current = 1;
  let lastEnd = 1;

  while (current <= slots) {
    const segment = rowSegments.find((s) => isSegmentInSlot(s, current));
    if (!segment?.event) {
      current++;
      continue;
    }

    const key = `_lvl_${current}`;
    const gap = Math.max(0, segment.left - lastEnd);

    let exactlyOneEvent = true;
    for (let slot = segment.left; slot < segment.left + segment.span; slot++) {
      if (eventsInSlot(segments, slot) !== 1) {
        exactlyOneEvent = false;
        break;
      }
    }
    if (!exactlyOneEvent) {
      const closureCurrent = current;
      if (gap) {
        const flexBasis = `${(Math.abs(gap) / slots) * 100}%`;
        row.push(
          <div key={`${key}_gap`} className="rbc-row-segment" style={{ flexBasis }} />,
        );
      }
      const count = eventsInSlot(segments, current);
      const flexBasis = `${(Math.abs(segment.span) / slots) * 100}%`;
      row.push(
        <div key={key} className="rbc-row-segment" style={{ flexBasis }}>
          {!count ? null : (
            <button
              type="button"
              key={`sm_${current}`}
              className="rbc-show-more text-inherit bg-transparent m-0 p-0 border-none cursor-pointer select-text"
              onClick={(e) => {
                e.preventDefault();
                e.stopPropagation();
                const date = slotMetrics.getDateForSlot(closureCurrent - 1);
                if (onShowMore) onShowMore(date);
                else onDrillDown?.(date);
              }}
            >
              {`+${count} dalších`}
            </button>
          )}
        </div>,
      );
      lastEnd = current = current + 1;
      continue;
    }

    if (gap) {
      const flexBasis = `${(Math.abs(gap) / slots) * 100}%`;
      row.push(
        <div key={`${key}_gap`} className="rbc-row-segment" style={{ flexBasis }} />,
      );
    }
    const flexBasis = `${(Math.abs(segment.span) / slots) * 100}%`;
    row.push(
      <div key={key} className="rbc-row-segment" style={{ flexBasis }}>
        <EventCell
          event={segment.event}
          continuesPrior={slotMetrics.continuesPrior(segment.event)}
          continuesAfter={slotMetrics.continuesAfter(segment.event)}
        />
      </div>,
    );
    lastEnd = current = segment.right + 1;
  }

  return <div className="rbc-row">{row}</div>;
}

export default EventEndingRow;
