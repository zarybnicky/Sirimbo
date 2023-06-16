import type { Bounds, Box, Event } from "./utils/constants"
import { Unit } from 'date-arithmetic'
import { add, eq, min, max, diff, isSameDate, startOf, inEventRange, ceil } from './localizer'

export function pointInColumn(bounds: Bounds, point: Box) {
  const { left, right, top } = bounds
  const { x, y } = point
  return x < right + 10 && x > left && y > top
}

export function eventTimes({ start, end }: Event) {
  const isZeroDuration = eq(start, end, 'minutes') && diff(start, end, 'minutes') === 0
  // make zero duration midnight events at least one day long
  if (isZeroDuration) end = add(end, 1, 'day')
  const duration = diff(start, end, 'milliseconds')
  return { start, end, duration }
}

export function endOfRange(dateRange: Date[], unit: Unit = 'day') {
  return {
    first: dateRange[0],
    last: add(dateRange[dateRange.length - 1], 1, unit),
  }
}

// properly calculating segments requires working with dates in
// the timezone we're working with, so we use the localizer

export type Segment = {
  event: Event;
  span: number;
  left: number;
  right: number;
}
export function eventSegments(event: Event, range: Date[]): Segment {
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

export function eventLevels(rowSegments: Segment[], limit = Infinity) {
  const levels: Segment[][] = [];
  const extra: Segment[] = []

  for (let i = 0; i < rowSegments.length; i++) {
    let seg = rowSegments[i]

    // Check for overlapping
    let j: number
    for (j = 0; j < levels.length; j++) {
      if (!levels[j].some((other) => other.left <= seg.right && other.right >= seg.left)) {
        break
      }
    }

    if (j >= limit) {
      extra.push(seg);
    } else {
      if (levels.length < j) {
        levels.push([]);
      }
      levels[j].push(seg);
    }
  }

  for (let i = 0; i < levels.length; i++) {
    levels[i].sort((a, b) => a.left - b.left)
  }

  return { levels, extra }
}
