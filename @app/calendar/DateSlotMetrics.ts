import { eventSegments, endOfRange, eventLevels } from './common'
import { eq, lt, gt, gte } from './localizer'
import { Event } from './types';

export type DateSlotMetrics = ReturnType<typeof getSlotMetrics>;

export const getSlotMetrics = ({ range, events, maxRows, minRows }: {
  range: Date[];
  events: Event[];
  maxRows: number;
  minRows: number;
}) => {
  let { first, last } = endOfRange(range)

  let segments = events.map((evt) => eventSegments(evt, range))

  let { levels, extra } = eventLevels(segments, Math.max(maxRows - 1, 1))
  // Subtract 1 from minRows to not include showMore button row when
  // it would be rendered
  const minEventRows = extra.length > 0 ? minRows - 1 : minRows
  while (levels.length < minEventRows) levels.push([])

  return {
    first,
    last,
    levels,
    extra,
    range,
    slots: range.length,

    getDateForSlot(slotNumber: number) {
      return range[slotNumber]!
    },

    getSlotForDate(date: Date) {
      return range.find((r) => eq(r, date, 'day'))
    },

    getEventsForSlot(slot: number) {
      return segments
        .filter((seg) => seg.left <= slot && seg.right >= slot)
        .map((seg) => seg.event)
    },

    continuesPrior({ start }: Event) {
      return lt(start, first, 'day')
    },

    continuesAfter({ start, end }: Event) {
      const singleDayDuration = eq(start, end, 'minutes')
      return singleDayDuration
        ? gte(end, last, 'minutes')
        : gt(end, last, 'minutes')
    },
  }
}
