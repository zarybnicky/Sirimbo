import type { CalendarEvent } from "./types"
import { add, eq, min, max, diff, startOf, ceil } from './localizer'

export function endOfRange(dateRange: Date[]) {
  return {
    first: dateRange[0]!,
    last: add(dateRange[dateRange.length - 1]!, 1, 'day'),
  }
}

export type Segment = {
  event: CalendarEvent;
  span: number;
  left: number;
  right: number;
}
export function eventSegments(event: CalendarEvent, range: Date[]): Segment {
  let { first, last } = endOfRange(range)

  let slots = diff(first, last, 'day')
  let start = max(startOf(event.start, 'day'), first)
  let end = min(ceil(event.end, 'day'), last)
  let padding = range.findIndex((x) => eq(x, start, 'day'))
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
    let seg = rowSegments[i]!

    // Check for overlapping
    let j: number
    for (j = 0; j < levels.length; j++) {
      if (!levels[j]!.some((other) => other.left <= seg.right && other.right >= seg.left)) {
        break
      }
    }

    if (j >= limit) {
      extra.push(seg);
    } else {
      if (levels.length <= j) {
        levels.push([]);
      }
      levels[j]!.push(seg);
    }
  }

  levels.forEach(arr => arr.sort((a, b) => a.left - b.left));

  return { levels, extra }
}
