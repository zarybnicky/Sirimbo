import memoize from 'memoize-one'
import { eventSegments, endOfRange, eventLevels } from './eventLevels'
import { isSameDate, continuesAfter, continuesPrior } from '../localizer'

export const getSlotMetrics = memoize((options) => {
  const { range, events, maxRows, minRows } = options
  let { first, last } = endOfRange({ dateRange: range })

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

    getDateForSlot(slotNumber) {
      return range[slotNumber]
    },

    getSlotForDate(date) {
      return range.find((r) => isSameDate(r, date))
    },

    getEventsForSlot(slot) {
      return segments
        .filter((seg) => seg.left <= slot && seg.right >= slot)
        .map((seg) => seg.event)
    },

    continuesPrior({ start }) {
      return continuesPrior(start, first)
    },

    continuesAfter({ start, end }) {
      return continuesAfter(start, end, last)
    },
  }
}, (a, b) => a[0].range === b[0].range && a[0].events === b[0].events)
