import { addDays, startOfDay, differenceInCalendarDays, subMilliseconds, min, max } from 'date-fns';
import type { CalendarEvent } from './types';

export function merge(date: Date, time: Date): Date {
  const result = new Date(date);
  result.setHours(
    time.getHours(),
    time.getMinutes(),
    time.getSeconds(),
    time.getMilliseconds(),
  );
  return result;
}

export function endOfRange(dateRange: Date[]) {
  return {
    first: dateRange[0]!,
    last: addDays(dateRange.at(-1)!, 1),
  };
}

export type Segment = {
  event: CalendarEvent;
  span: number;
  left: number;
  right: number;
};

export function eventSegments(event: CalendarEvent, range: Date[]): Segment {
  const { first, last } = endOfRange(range);

  const start = max([startOfDay(event.start), first]);
  const end = subMilliseconds(min([event.end, last]), 1);
  const padding = differenceInCalendarDays(start, first);
  const span = Math.max(1, differenceInCalendarDays(end, start) + 1);

  return {
    event,
    span: Math.min(span, range.length),
    left: padding + 1,
    right: Math.max(padding + span, 1),
  };
}

export function eventLevels(rowSegments: Segment[], limit = Number.POSITIVE_INFINITY) {
  const levels: Segment[][] = [];
  const extra: Segment[] = [];

  for (const segment of rowSegments) {
    const available = levels.findIndex((level) =>
      level.every((other) => other.left > segment.right || other.right < segment.left),
    );
    const index = available === -1 ? levels.length : available;

    if (index >= limit) {
      extra.push(segment);
    } else if (index === levels.length) {
      levels.push([segment]);
    } else {
      levels[index]!.push(segment);
    }
  }

  for (const arr of levels) arr.sort((a, b) => a.left - b.left);

  return { levels, extra };
}

export function eventInRange(
  event: { start: Date; end: Date },
  range: { start: Date; end: Date },
) {
  return (
    event.start < addDays(startOfDay(range.end), 1) &&
    (event.end > startOfDay(range.start) ||
      (+event.start === +event.end && event.start >= startOfDay(range.start)))
  );
}
