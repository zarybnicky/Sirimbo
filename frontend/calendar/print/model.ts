import type { CalendarInstanceEvent, Resource } from '@/calendar/types';
import { eq, startOf } from 'date-arithmetic';
import { formatDefaultInstanceName } from '@/ui/format';

/**
 * Model for the print month overview only. The day and week views reuse the
 * calendar's own time-grid (`TimeGrid` → `getSlotMetrics` + `layoutEvents`),
 * which already handles overlapping lessons and proportional durations; a
 * table can't. The month view is the one layout the calendar doesn't already
 * have, so it lives here.
 */

export type OccupancySegment = {
  /** Position within the shared daily window, in percent. */
  startPct: number;
  widthPct: number;
  booked: boolean;
  label: string;
};

export type TrainerStrip = {
  resource: Resource;
  segments: OccupancySegment[];
};

export type MonthDay = {
  date: Date;
  groups: CalendarInstanceEvent[];
  strips: TrainerStrip[];
};

const isGroup = (e: CalendarInstanceEvent) => e.instance.type !== 'LESSON';
const isBooked = (e: CalendarInstanceEvent) =>
  (e.instance.registrations.totalCount ?? 0) > 0;

/**
 * Builds the trimmed month overview: for every day, its group lessons plus one
 * occupancy strip per trainer — solid where booked, hatched where free.
 * Segments share one daily time window (derived across the whole month) so the
 * strips line up visually across days and trainers.
 */
export function buildMonthDays(
  events: readonly CalendarInstanceEvent[],
  days: readonly Date[],
  resources: readonly Resource[],
): MonthDay[] {
  let min = Number.POSITIVE_INFINITY;
  let max = Number.NEGATIVE_INFINITY;
  for (const e of events) {
    min = Math.min(min, minutesOfDay(e.start));
    max = Math.max(max, minutesOfDay(e.end));
  }
  if (!Number.isFinite(min) || max <= min) {
    min = 8 * 60;
    max = 22 * 60;
  }
  const windowMinutes = max - min;

  return days.map((date) => {
    const dayEvents = events
      .filter((e) => eq(e.start, date, 'day'))
      .toSorted((a, b) => +a.start - +b.start);
    const lessons = dayEvents.filter((e) => !isGroup(e));
    const present = new Set(lessons.flatMap((e) => e.resourceIds));

    const strips = resources
      .filter((r) => present.has(r.resourceId))
      .map<TrainerStrip>((resource) => ({
        resource,
        segments: lessons
          .filter((e) => e.resourceIds.includes(resource.resourceId))
          .map<OccupancySegment>((e) => {
            const start = Math.max(min, minutesOfDay(e.start));
            const end = Math.min(max, minutesOfDay(e.end));
            return {
              startPct: ((start - min) / windowMinutes) * 100,
              widthPct: (Math.max(end - start, 6) / windowMinutes) * 100,
              booked: isBooked(e),
              label: formatDefaultInstanceName(e.instance),
            };
          }),
      }));

    return { date, groups: dayEvents.filter(isGroup), strips };
  });
}

function minutesOfDay(d: Date): number {
  return (+d - +startOf(d, 'day')) / 60_000;
}
