import { Heap } from 'heap-typed';
import type { CalendarEvent } from './types';
import type { TimeSlotMetrics } from './TimeSlotMetrics';

const MIN = 60_000;

type Item = {
  id: number;
  event: CalendarEvent;
  since: number; // ms (clamped to visible via slotMetrics)
  until: number; // ms (clamped)
  top: number; // %
  height: number; // %
  lane: number;
};

type LayoutMeta = {
  k: number; // max concurrency during this event
  lane: number;
  laneCount: number;
};

function lowerBoundByStart(arr: Item[], s: number) {
  let lo = 0;
  let hi = arr.length;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    if (arr[mid]!.since < s) lo = mid + 1;
    else hi = mid;
  }
  return lo;
}

// Overlap semantics identical to your original code (epsMs reduces overlaps).
function overlapsWithEps(a: Item, b: Item, epsMs: number) {
  return a.since < b.until - epsMs && b.since < a.until - epsMs;
}

class SegmentTreeMax {
  private n: number;
  private tree: number[];

  constructor(values: number[]) {
    let n = 1;
    while (n < values.length) n <<= 1;
    this.n = n;
    this.tree = Array.from({ length: 2 * n }, () => 0);
    for (let i = 0; i < values.length; i++) this.tree[n + i] = values[i]!;
    for (let i = n - 1; i > 0; i--) {
      this.tree[i] = Math.max(this.tree[i << 1]!, this.tree[(i << 1) | 1]!);
    }
  }

  query(lIncl: number, rIncl: number): number {
    if (lIncl > rIncl) return 1;
    let l = lIncl + this.n;
    let r = rIncl + this.n;
    let res = 0;
    while (l <= r) {
      if (l & 1) res = Math.max(res, this.tree[l++]!);
      if (!(r & 1)) res = Math.max(res, this.tree[r--]!);
      l >>= 1;
      r >>= 1;
    }
    return Math.max(1, res);
  }
}

export function layoutEvents(
  events: CalendarEvent[],
  slotMetrics: TimeSlotMetrics,
  minimumStartDifference: number,
): {
  event: CalendarEvent;
  style: { top: number; width: number; height: number; xOffset: number };
}[] {
  const epsMs = (minimumStartDifference ?? 0) * MIN;

  const items: Item[] = events
    .map((event, id) => {
      const r = slotMetrics.getRange(event.start, event.end);
      return {
        id,
        event,
        since: +r.startDate,
        until: +r.endDate,
        top: r.top,
        height: r.height,
        lane: -1,
      };
    })
    // start asc, longer first for same start
    .toSorted((a, b) => a.since - b.since || b.until - a.until);

  const out: {
    event: CalendarEvent;
    style: { top: number; width: number; height: number; xOffset: number };
    meta: LayoutMeta;
  }[] = [];

  const finalizeGroup = (group: Item[]) => {
    if (group.length === 0) return;

    // ---- A) Lane assignment (interval graph coloring; same idea as your original) ----
    const activeByEnd = new Heap<{ end: number; lane: number }>([], {
      comparator: (a, b) => a.end - b.end,
    });
    const freeLanes = new Heap<number>([], { comparator: (a, b) => a - b });

    let nextLane = 0;

    for (const it of group) {
      // same semantics as your original:
      // free if previous.until <= current.since + epsMs  <=> previous.until - epsMs <= current.since
      while (!activeByEnd.isEmpty() && activeByEnd.peek()!.end <= it.since + epsMs) {
        freeLanes.add(activeByEnd.poll()!.lane);
      }
      const lane = freeLanes.isEmpty() ? nextLane++ : freeLanes.poll()!;
      it.lane = lane;
      activeByEnd.add({ end: it.until, lane });
    }

    const laneCount = Math.max(1, nextLane);

    // per-lane arrays, sorted by start
    const lanes: Item[][] = Array.from({ length: laneCount }, () => []);
    for (const it of group) lanes[it.lane]!.push(it);
    for (const lane of lanes) lane.sort((a, b) => a.since - b.since || b.until - a.until);

    // ---- B) Policy 2: k(event) = max concurrency during the event ----
    // Use "effective end" = (until - epsMs) for clique/concurrency consistency with overlap test.
    const boundariesSet = new Set<number>();
    const effSince: number[] = Array.from({ length: group.length }, () => 0);
    const effUntil: number[] = Array.from({ length: group.length }, () => 0);

    for (const [i, it] of group.entries()) {
      const a = it.since;
      const b = Math.max(a, it.until - epsMs);
      effSince[i] = a;
      effUntil[i] = b;
      boundariesSet.add(a);
      boundariesSet.add(b);
    }

    const boundaries = [...boundariesSet].toSorted((a, b) => a - b);
    const segCount = Math.max(0, boundaries.length - 1);

    const idxOf = new Map<number, number>();
    for (let i = 0; i < boundaries.length; i++) idxOf.set(boundaries[i]!, i);

    const diff = Array.from({ length: boundaries.length + 1 }, () => 0);
    const sIdx: number[] = Array.from({ length: group.length }, () => 0);
    const eIdx: number[] = Array.from({ length: group.length }, () => 0);

    for (let i = 0; i < group.length; i++) {
      const s = idxOf.get(effSince[i]!)!;
      const e = idxOf.get(effUntil[i]!)!;
      sIdx[i] = s;
      eIdx[i] = e;
      if (s < e) {
        diff[s]! += 1;
        diff[e]! -= 1;
      }
    }

    const segConcurrency: number[] = Array.from({ length: segCount }, () => 0);
    let active = 0;
    for (let i = 0; i < segCount; i++) {
      active += diff[i]!;
      segConcurrency[i] = active;
    }

    const st = new SegmentTreeMax(segConcurrency);

    const kOfItemId = new Map<number, number>();
    for (const [i, element] of group.entries()) {
      const s = sIdx[i]!;
      const e = eIdx[i]!;
      const k = s < e ? st.query(s, e - 1) : 1;
      kOfItemId.set(element.id, k);
    }

    // ---- C) Offline xOffset: lane-by-lane constraints (prevents “holes” between parallel events) ----
    const placed = new Map<number, { x: number; w: number }>();

    const maxRightEdgeFromLane = (lane: Item[], it: Item): number => {
      // lane has non-overlapping events (by definition), but it can have multiple overlaps with a long it
      let maxRight = 0;

      // scan around start; then move forward while overlaps are still possible
      let i = lowerBoundByStart(lane, it.since);
      if (i > 0) i--;

      for (; i < lane.length; i++) {
        const other = lane[i]!;
        // if other starts after it ends (with eps), we can stop
        if (other.since >= it.until - epsMs) break;

        if (overlapsWithEps(it, other, epsMs)) {
          const p = placed.get(other.id);
          if (p) maxRight = Math.max(maxRight, p.x + p.w);
        }
      }

      return maxRight;
    };

    for (let l = 0; l < laneCount; l++) {
      for (const it of lanes[l]!) {
        const k = kOfItemId.get(it.id) ?? 1;
        const w = 100 / Math.max(1, k);

        let x = 0;
        if (l > 0) {
          for (let ll = 0; ll < l; ll++) {
            x = Math.max(x, maxRightEdgeFromLane(lanes[ll]!, it));
          }
        }

        placed.set(it.id, { x, w });

        out.push({
          event: it.event,
          style: { top: it.top, height: it.height, xOffset: x, width: w },
          meta: { k, lane: it.lane, laneCount },
        });
      }
    }
  };

  // Partition into overlap-connected groups with rolling max end (same as your original)
  let group: Item[] = [];
  let groupEnd = -Infinity;

  for (const it of items) {
    if (group.length === 0) {
      group = [it];
      groupEnd = it.until;
      continue;
    }

    if (it.since >= groupEnd - epsMs) {
      finalizeGroup(group);
      group = [it];
      groupEnd = it.until;
    } else {
      group.push(it);
      groupEnd = Math.max(groupEnd, it.until);
    }
  }
  if (group.length > 0) finalizeGroup(group);

  // Final render order: top asc, longer first (shorter later => on top)
  out.sort((a, b) => {
    if (a.style.top !== b.style.top) return a.style.top - b.style.top;
    return b.style.top + b.style.height - (a.style.top + a.style.height);
  });

  return out.map(({ event, style }) => ({ event, style }));
}
