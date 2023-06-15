import findIndex from 'lodash/findIndex'
import localizer from './localizer'

export function endOfRange({ dateRange, unit = 'day' }) {
  return {
    first: dateRange[0],
    last: localizer.add(dateRange[dateRange.length - 1], 1, unit),
  }
}

// properly calculating segments requires working with dates in
// the timezone we're working with, so we use the localizer
export function eventSegments(event, range) {
  let { first, last } = endOfRange({ dateRange: range })

  let slots = localizer.diff(first, last, 'day')
  let start = localizer.max(
    localizer.startOf(event.start, 'day'),
    first
  )
  let end = localizer.min(localizer.ceil(event.end, 'day'), last)

  let padding = findIndex(range, (x) => localizer.isSameDate(x, start))
  let span = localizer.diff(start, end, 'day')

  span = Math.min(span, slots)
  // The segmentOffset is necessary when adjusting for timezones
  // ahead of the browser timezone
  span = Math.max(span - localizer.segmentOffset, 1)

  return {
    event,
    span,
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
  return localizer.inEventRange({ event, range })
}

export function segsOverlap(seg, otherSegs) {
  return otherSegs.some(
    (otherSeg) => otherSeg.left <= seg.right && otherSeg.right >= seg.left
  )
}

export function sortEvents(eventA, eventB) {
  const evtA = {
    start: eventA.start,
    end: eventA.end,
    allDay: eventA.allDay,
  }
  const evtB = {
    start: eventB.start,
    end: eventB.end,
    allDay: eventB.allDay,
  }
  return localizer.sortEvents({ evtA, evtB })
}
