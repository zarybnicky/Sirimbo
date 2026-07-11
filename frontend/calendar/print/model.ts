import type { CalendarEvent, CalendarInstanceEvent } from '@/calendar/types';
import { eq, startOf } from 'date-arithmetic';
import { formatEventType, formatRegistrant } from '@/ui/format';

/**
 * Shared data model for the print-ready schedule views. The interactive
 * calendar positions events absolutely for drag-and-drop; the printed views
 * instead need static, paper-friendly tables and summaries, so they work off
 * this small set of pure helpers rather than the on-screen layout engine.
 */

const UNASSIGNED_ID = '__none__';

export type Trainer = { id: string; name: string };

export type CellKind = 'group' | 'booked' | 'free';

export function instanceEvents(
  events: readonly CalendarEvent[],
): CalendarInstanceEvent[] {
  return events.filter((e): e is CalendarInstanceEvent => e.kind === 'event');
}

export function eventsOnDay(
  events: readonly CalendarInstanceEvent[],
  day: Date,
): CalendarInstanceEvent[] {
  return events
    .filter((e) => eq(e.start, day, 'day'))
    .toSorted((a, b) => +a.start - +b.start);
}

function isGroup(e: CalendarInstanceEvent): boolean {
  return e.instance.type !== 'LESSON';
}

function isBooked(e: CalendarInstanceEvent): boolean {
  return (e.instance.registrations.totalCount ?? 0) > 0;
}

export function cellKind(e: CalendarInstanceEvent): CellKind {
  if (isGroup(e)) return 'group';
  return isBooked(e) ? 'booked' : 'free';
}

/** The trainer column(s) an event belongs to, or the unassigned bucket. */
function columnIdsFor(e: CalendarInstanceEvent): string[] {
  const ids = (e.instance.trainersList ?? [])
    .map((t) => t.personId)
    .filter((id): id is string => !!id);
  return ids.length > 0 ? ids : [UNASSIGNED_ID];
}

/** Distinct trainers appearing in the events, sorted by name, with an
 * "Ostatní" bucket appended when some events have no trainer assigned. */
function collectColumns(events: CalendarInstanceEvent[]): Trainer[] {
  const map = new Map<string, string>();
  let hasUnassigned = false;
  for (const e of events) {
    const trainers = (e.instance.trainersList ?? []).filter((t) => t.personId);
    if (trainers.length === 0) hasUnassigned = true;
    for (const t of trainers) map.set(t.personId, t.person?.name || '');
  }
  const columns = [...map.entries()]
    .map(([id, name]) => ({ id, name }))
    .toSorted((a, b) => a.name.localeCompare(b.name, 'cs'));
  if (hasUnassigned) columns.push({ id: UNASSIGNED_ID, name: 'Ostatní' });
  return columns;
}

/** The label shown inside a schedule cell: an explicit name, the booked
 * registrants, the group/event type, or a "free lesson" marker. */
export function eventLabel(e: CalendarInstanceEvent): string {
  const { instance } = e;
  if (instance.name) return instance.name;
  if (instance.type === 'LESSON') {
    const regs = instance.registrations.nodes;
    if (regs.length > 0) return regs.map(formatRegistrant).filter(Boolean).join(', ');
    return 'volná';
  }
  return formatEventType(instance.type);
}

export function cohortColor(e: CalendarInstanceEvent): string | undefined {
  return (
    e.instance.targetCohortsList?.find((x) => x.cohort?.colorRgb)?.cohort
      ?.colorRgb || undefined
  );
}

// ---- Day grid ----------------------------------------------------------

export type DayGridRow = { start: Date; end: Date };

export type DayCell =
  | { type: 'empty' }
  | { type: 'span' } // covered by an event that renders in an earlier row
  | { type: 'event'; event: CalendarInstanceEvent; rowSpan: number };

export type DayGrid = {
  columns: Trainer[];
  rows: DayGridRow[];
  /** cells[columnIndex][rowIndex] */
  cells: DayCell[][];
  isEmpty: boolean;
};

/**
 * Builds a trainers-as-columns / time-as-rows table for a single day.
 *
 * Rows are derived from the distinct start/end boundaries of that day's
 * lessons (as in the club's hand-kept spreadsheet), so the table compacts to
 * only the times something actually happens while still letting a longer
 * lesson span the shorter slots underneath it.
 */
export function buildDayGrid(dayEvents: CalendarInstanceEvent[]): DayGrid {
  const columns = collectColumns(dayEvents);

  const boundaries = [
    ...new Set(dayEvents.flatMap((e) => [+e.start, +e.end])),
  ].toSorted((a, b) => a - b);
  const rows: DayGridRow[] = [];
  for (let i = 0; i < boundaries.length - 1; i++) {
    rows.push({ start: new Date(boundaries[i]!), end: new Date(boundaries[i + 1]!) });
  }

  const colIdx = new Map(columns.map((c, i) => [c.id, i]));
  const cells: DayCell[][] = columns.map(() =>
    rows.map(() => ({ type: 'empty' }) as DayCell),
  );

  for (const e of dayEvents) {
    const r0 = rows.findIndex((r) => +r.start >= +e.start && +r.start < +e.end);
    if (r0 === -1) continue;
    let span = 0;
    while (r0 + span < rows.length && +rows[r0 + span]!.start < +e.end) span++;

    for (const cid of columnIdsFor(e)) {
      const ci = colIdx.get(cid);
      if (ci === undefined || cells[ci]![r0]!.type !== 'empty') continue;
      cells[ci]![r0] = { type: 'event', event: e, rowSpan: span };
      for (let r = r0 + 1; r < r0 + span; r++) cells[ci]![r] = { type: 'span' };
    }
  }

  return { columns, rows, cells, isEmpty: dayEvents.length === 0 };
}

// ---- Month occupancy ---------------------------------------------------

export type OccupancySegment = {
  /** Fraction of the day window [0, 1] where the segment starts/ends. */
  startPct: number;
  widthPct: number;
  kind: CellKind;
  label: string;
};

export type TrainerDay = {
  trainer: Trainer;
  segments: OccupancySegment[];
  hasFree: boolean;
  hasBooked: boolean;
};

export type MonthDay = {
  date: Date;
  groups: CalendarInstanceEvent[];
  trainerDays: TrainerDay[];
};

/**
 * Builds the trimmed month overview: for every day, the group lessons written
 * out plus one occupancy strip per trainer. Segments are placed against a
 * shared daily time window so the strips line up visually across trainers.
 */
export function buildMonthDays(
  monthEvents: CalendarInstanceEvent[],
  days: Date[],
): MonthDay[] {
  // A single time window shared by every strip, derived from the whole month
  // so bars are comparable day-to-day. Falls back to a sensible default when
  // the month is empty.
  let min = Number.POSITIVE_INFINITY;
  let max = Number.NEGATIVE_INFINITY;
  for (const e of monthEvents) {
    const s = minutesOfDay(e.start);
    const u = minutesOfDay(e.end);
    if (s < min) min = s;
    if (u > max) max = u;
  }
  if (!Number.isFinite(min) || !Number.isFinite(max) || max <= min) {
    min = 8 * 60;
    max = 22 * 60;
  }
  const windowMinutes = max - min;

  return days.map((date) => {
    const dayEvents = eventsOnDay(monthEvents, date);
    const groups = dayEvents.filter(isGroup);
    const columns = collectColumns(dayEvents.filter((e) => !isGroup(e)));

    const trainerDays: TrainerDay[] = columns.map((trainer) => {
      const own = dayEvents.filter(
        (e) => !isGroup(e) && columnIdsFor(e).includes(trainer.id),
      );
      const segments = own.map<OccupancySegment>((e) => {
        const start = Math.max(min, minutesOfDay(e.start));
        const end = Math.min(max, minutesOfDay(e.end));
        return {
          startPct: ((start - min) / windowMinutes) * 100,
          widthPct: (Math.max(end - start, 6) / windowMinutes) * 100,
          kind: cellKind(e),
          label: eventLabel(e),
        };
      });
      return {
        trainer,
        segments,
        hasFree: own.some((e) => !isBooked(e)),
        hasBooked: own.some(isBooked),
      };
    });

    return { date, groups, trainerDays };
  });
}

function minutesOfDay(d: Date): number {
  const day = startOf(d, 'day');
  return (+d - +day) / 60_000;
}
