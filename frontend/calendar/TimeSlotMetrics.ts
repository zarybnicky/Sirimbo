import { merge } from './localizer';

export type TimeSlotMetrics = ReturnType<typeof getSlotMetrics>;

const MIN = 60_000;

export function getSlotMetrics({
  date,
  minTime,
  maxTime,
  step,
  timeslots,
}: {
  date: Date;
  minTime: Date;
  maxTime: Date;
  step: number;
  timeslots: number;
}) {
  const visibleStart = merge(date, minTime);
  const visibleEnd = merge(date, maxTime);

  const startMs = +visibleStart;
  const endMs = +visibleEnd;
  const rangeMs = Math.max(1, endMs - startMs);
  const stepMs = step * MIN;

  const clampMs = (t: number) => Math.min(endMs, Math.max(startMs, t));
  const topPctFromMs = (t: number) => ((clampMs(t) - startMs) / rangeMs) * 100;

  // Pad to whole groups (keeps old “groups + slots” behavior)
  const rawSlots = Math.ceil(rangeMs / stepMs);
  const numGroups = Math.ceil(rawSlots / timeslots);
  const numSlots = numGroups * timeslots;

  const slotAt = (slotIdx: number) => new Date(startMs + slotIdx * stepMs);

  const groups: Date[][] = Array.from({ length: numGroups }, (_, g) =>
    Array.from({ length: timeslots }, (_, s) => slotAt(g * timeslots + s)),
  );

  // includes “one extra”, selectable to end
  const slots: Date[] = Array.from({ length: numSlots + 1 }, (_, i) => slotAt(i));

  return {
    groups,

    dateIsInGroup(date: Date, groupIndex: number): boolean {
      const g = groups[groupIndex];
      if (!g) return false;
      const next = groups[groupIndex + 1]?.[0] ?? visibleEnd;
      return +date >= +g[0]! && +date < +next;
    },

    nextSlot(slot: Date): Date {
      const t = clampMs(+slot);
      const base = Math.floor((t - startMs) / stepMs);
      const idx = Math.min(slots.length - 1, base + 1);
      return slots[idx]!;
    },

    closestSlotToPosition(percent: number): Date {
      const p = Math.min(1, Math.max(0, percent));
      const idx = Math.min(slots.length - 1, Math.max(0, Math.floor(p * numSlots)));
      return slots[idx]!;
    },

    closestSlotFromPoint(
      point: { x: number; y: number },
      boundaryRect: { top: number; bottom: number },
    ): Date {
      const rangePx = Math.abs(boundaryRect.top - boundaryRect.bottom) || 1;
      return this.closestSlotToPosition((point.y - boundaryRect.top) / rangePx);
    },

    closestSlotFromDate(date: Date, offset = 0): Date {
      if (+date <= startMs) return slots[0]!;
      if (+date >= endMs) return slots.at(-1)!;

      const t = clampMs(+date);
      const base = Math.floor((t - startMs) / stepMs);
      const idx = Math.min(slots.length - 1, Math.max(0, base + offset));
      return slots[idx]!;
    },

    startsBeforeDay(d: Date): boolean {
      const dayStart = new Date(d.getFullYear(), d.getMonth(), d.getDate(), 0, 0, 0, 0);
      return +d < +dayStart;
    },
    startsAfterDay(d: Date): boolean {
      const dayEnd = new Date(
        d.getFullYear(),
        d.getMonth(),
        d.getDate(),
        23,
        59,
        59,
        999,
      );
      return +d > +dayEnd;
    },

    startsBefore(date: Date): boolean {
      return +merge(visibleStart, date) < startMs;
    },
    startsAfter(date: Date): boolean {
      return +merge(visibleEnd, date) > endMs;
    },

    getRange(rangeStart: Date, rangeEnd: Date, ignoreMin = false, ignoreMax = false) {
      let a = +rangeStart;
      let b = +rangeEnd;
      if (b < a) [a, b] = [b, a];

      if (!ignoreMin) a = Math.max(startMs, Math.min(endMs, a));
      if (!ignoreMax) b = Math.max(startMs, Math.min(endMs, b));

      const top = topPctFromMs(a);
      const endTop = topPctFromMs(b);
      const height = Math.max(0, endTop - top);

      return {
        top,
        height,
        start: (a - startMs) / MIN,
        startDate: new Date(a),
        end: (b - startMs) / MIN,
        endDate: new Date(b),
      };
    },

    getCurrentTimePosition(rangeStart: Date): number {
      return topPctFromMs(+rangeStart);
    },
  };
}
