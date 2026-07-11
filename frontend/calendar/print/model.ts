import type { CalendarInstanceEvent, Resource } from '@/calendar/types';
import { eq, startOf } from 'date-arithmetic';
import { formatEventType, formatRegistrant } from '@/ui/format';

/**
 * Data model for the print-ready schedule views. Trainer columns come from the
 * calendar's existing resource grouping (`useCalendarData(..., 'trainer')`);
 * this module adds the paper-only pieces: a dedicated "groups" lane, a
 * boundary-based day table, and the month occupancy strips.
 */

export type CellInfo = {
  isGroup: boolean;
  isBooked: boolean;
  /** What to print inside the cell. */
  label: string;
  /** Group colour taken from the target cohort, if any. */
  color?: string;
};

/** Derives everything the views need to render a single instance. */
export function describeEvent(e: CalendarInstanceEvent): CellInfo {
  const { instance } = e;
  const isGroup = instance.type !== 'LESSON';
  const isBooked = (instance.registrations.totalCount ?? 0) > 0;
  const color =
    instance.targetCohortsList?.find((x) => x.cohort?.colorRgb)?.cohort?.colorRgb ||
    undefined;

  const label =
    instance.name ||
    (isGroup
      ? formatEventType(instance.type)
      : isBooked
        ? instance.registrations.nodes.map(formatRegistrant).filter(Boolean).join(', ')
        : 'volná');

  return { isGroup, isBooked, label, color };
}

// ---- Day grid ----------------------------------------------------------

export type PrintColumn = {
  key: string;
  title: string;
  kind: 'group' | 'trainer';
};

export type DayGridRow = { start: Date; end: Date };

export type DayCell =
  | { type: 'empty' }
  | { type: 'span' } // covered by an event that renders in an earlier row
  | { type: 'event'; event: CalendarInstanceEvent; rowSpan: number };

export type DayGrid = {
  columns: PrintColumn[];
  /** Leading columns holding group lessons; the rest are trainers. */
  groupColumnCount: number;
  rows: DayGridRow[];
  /** cells[columnIndex][rowIndex] */
  cells: DayCell[][];
  isEmpty: boolean;
};

/**
 * Builds the day table: group lessons packed into as few side-by-side lanes as
 * their overlaps require, followed by one column per trainer with events that
 * day. Rows are the distinct start/end boundaries of the day's events, so the
 * table compacts to the times something happens while a longer lesson spans
 * the shorter slots beneath it.
 */
export function buildDayGrid(
  events: readonly CalendarInstanceEvent[],
  day: Date,
  resources: readonly Resource[],
): DayGrid {
  const dayEvents = eventsForDay(events, day);
  const groupLanes = packLanes(dayEvents.filter((e) => describeEvent(e).isGroup));
  const lessons = dayEvents.filter((e) => !describeEvent(e).isGroup);

  const present = new Set(lessons.flatMap((e) => e.resourceIds));
  const trainerCols = resources.filter((r) => present.has(r.resourceId));

  const columns: PrintColumn[] = [
    ...groupLanes.map((_, i) => ({
      key: `__group_${i}`,
      title: 'Společné',
      kind: 'group' as const,
    })),
    ...trainerCols.map((r) => ({
      key: r.resourceId,
      title: r.resourceTitle,
      kind: 'trainer' as const,
    })),
  ];

  // Which column(s) each event lives in.
  const columnKeys = new Map<CalendarInstanceEvent, string[]>();
  for (const [i, lane] of groupLanes.entries())
    for (const e of lane) columnKeys.set(e, [`__group_${i}`]);
  for (const e of lessons) columnKeys.set(e, [...e.resourceIds]);

  const boundaries = [
    ...new Set(dayEvents.flatMap((e) => [+e.start, +e.end])),
  ].toSorted((a, b) => a - b);
  const rows: DayGridRow[] = [];
  for (let i = 0; i < boundaries.length - 1; i++) {
    rows.push({ start: new Date(boundaries[i]!), end: new Date(boundaries[i + 1]!) });
  }

  const colIdx = new Map(columns.map((c, i) => [c.key, i]));
  const cells: DayCell[][] = columns.map(() =>
    rows.map(() => ({ type: 'empty' }) as DayCell),
  );

  for (const e of dayEvents) {
    const r0 = rows.findIndex((r) => +r.start >= +e.start && +r.start < +e.end);
    if (r0 === -1) continue;
    let span = 0;
    while (r0 + span < rows.length && +rows[r0 + span]!.start < +e.end) span++;

    for (const key of columnKeys.get(e) ?? []) {
      const ci = colIdx.get(key);
      if (ci === undefined || cells[ci]![r0]!.type !== 'empty') continue;
      cells[ci]![r0] = { type: 'event', event: e, rowSpan: span };
      for (let r = r0 + 1; r < r0 + span; r++) cells[ci]![r] = { type: 'span' };
    }
  }

  return {
    columns,
    groupColumnCount: groupLanes.length,
    rows,
    cells,
    isEmpty: dayEvents.length === 0,
  };
}

/** Greedy interval partition: the fewest lanes so no two events overlap in a
 * lane. Input must be sorted by start. */
function packLanes(events: CalendarInstanceEvent[]): CalendarInstanceEvent[][] {
  const lanes: { end: number; items: CalendarInstanceEvent[] }[] = [];
  for (const e of events) {
    const lane = lanes.find((l) => l.end <= +e.start);
    if (lane) {
      lane.items.push(e);
      lane.end = +e.end;
    } else {
      lanes.push({ end: +e.end, items: [e] });
    }
  }
  return lanes.map((l) => l.items);
}

// ---- Month occupancy ---------------------------------------------------

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

/**
 * Builds the trimmed month overview: for every day, its group lessons plus one
 * occupancy strip per resource — solid where booked, hatched where free.
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
    const dayEvents = eventsForDay(events, date);
    const groups = dayEvents.filter((e) => describeEvent(e).isGroup);
    const lessons = dayEvents.filter((e) => !describeEvent(e).isGroup);
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
              booked: describeEvent(e).isBooked,
              label: describeEvent(e).label,
            };
          }),
      }));

    return { date, groups, strips };
  });
}

function eventsForDay(events: readonly CalendarInstanceEvent[], day: Date) {
  return events
    .filter((e) => eq(e.start, day, 'day'))
    .toSorted((a, b) => +a.start - +b.start);
}

function minutesOfDay(d: Date): number {
  return (+d - +startOf(d, 'day')) / 60_000;
}
