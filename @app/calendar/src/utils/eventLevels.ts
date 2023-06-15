import { Unit } from 'date-arithmetic'
import { add, min, max, diff, isSameDate, startOf, inEventRange, ceil } from '../localizer'

export function endOfRange(dateRange: Date[], unit: Unit = 'day') {
  return {
    first: dateRange[0],
    last: add(dateRange[dateRange.length - 1], 1, unit),
  }
}

// properly calculating segments requires working with dates in
// the timezone we're working with, so we use the localizer
export function eventSegments(event, range) {
  let { first, last } = endOfRange(range)

  let slots = diff(first, last, 'day')
  let start = max(startOf(event.start, 'day'), first)
  let end = min(ceil(event.end, 'day'), last)
  let padding = range.findIndex((x) => isSameDate(x, start))
  let span = diff(start, end, 'day')

  return {
    event,
    span: Math.max(Math.min(span, slots), 1),
    left: padding + 1,
    right: Math.max(padding + span, 1),
  }
}

export function eventLevels(rowSegments, limit = Infinity) {
  let i,
    j,
    seg,
    levels = [],
    extra = []

  for (i = 0; i < rowSegments.length; i++) {
    seg = rowSegments[i]

    for (j = 0; j < levels.length; j++) if (!segsOverlap(seg, levels[j])) break

    if (j >= limit) {
      extra.push(seg)
    } else {
      ;(levels[j] || (levels[j] = [])).push(seg)
    }
  }

  for (i = 0; i < levels.length; i++) {
    levels[i].sort((a, b) => a.left - b.left) //eslint-disable-line
  }

  return { levels, extra }
}

export function inRange(e, start, end) {
  const event = { start: e.start, end: e.end };
  const range = { start, end };
  return inEventRange({ event, range })
}

export function segsOverlap(seg, others) {
  return others.some((other) => other.left <= seg.right && other.right >= seg.left)
}
