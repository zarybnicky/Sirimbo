import { eventSegments, endOfRange, eventLevels } from './eventLevels'
import { isSameDate, continuesAfter, continuesPrior } from '../localizer'
import { Event } from './constants';

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
      return range[slotNumber]
    },

    getSlotForDate(date: Date) {
      return range.find((r) => isSameDate(r, date))
    },

    getEventsForSlot(slot: number) {
      return segments
        .filter((seg) => seg.left <= slot && seg.right >= slot)
        .map((seg) => seg.event)
    },

    continuesPrior({ start }: Event) {
      return continuesPrior(start, first)
    },

    continuesAfter({ start, end }: Event) {
      return continuesAfter(start, end, last)
    },
  }
}
