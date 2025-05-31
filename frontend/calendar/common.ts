import type { CalendarEvent } from "./types"
import { diff, ceil } from './localizer'
import { add, eq, min, max, startOf } from 'date-arithmetic';

export function endOfRange(dateRange: Date[]) {
  return {
    first: dateRange[0]!,
    last: add(dateRange.at(-1)!, 1, 'day'),
  }
}

export type Segment = {
  event: CalendarEvent;
  span: number;
  left: number;
  right: number;
}
export function eventSegments(event: CalendarEvent, range: Date[]): Segment {
  const { first, last } = endOfRange(range)

  const slots = diff(first, last, 'day')
  const start = max(startOf(event.start, 'day'), first)
  const end = min(ceil(event.end, 'day'), last)
  const padding = range.findIndex((x) => eq(x, start, 'day'))
  const span = diff(start, end, 'day')

  return {
    event,
    span: Math.max(Math.min(span, slots), 1),
    left: padding + 1,
    right: Math.max(padding + span, 1),
  }
}

export function eventLevels(rowSegments: Segment[], limit = Number.POSITIVE_INFINITY) {
  const levels: Segment[][] = [];
  const extra: Segment[] = []

  for (const rowSegment of rowSegments) {
    const seg = rowSegment!

    // Check for overlapping
    let j: number
    for (j = 0; j < levels.length; j++) {
      if (!levels[j]?.some((other) => other.left <= seg.right && other.right >= seg.left)) {
        break
      }
    }

    if (j >= limit) {
      extra.push(seg);
    } else {
      if (levels.length <= j) {
        levels.push([]);
      }
      levels[j]?.push(seg);
    }
  }

  for (const arr of levels)
    arr.sort((a, b) => a.left - b.left);

  return { levels, extra }
}
