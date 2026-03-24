import type { CalendarEvent } from './types';
import type { TimeSlotMetrics } from './TimeSlotMetrics';

const MIN = 60_000;

type Item = {
  id: number;
  event: CalendarEvent;
  since: number; // ms (clamped to visible range)
  until: number; // ms (clamped)
  top: number; // %
  height: number; // %
  lane: number;
};

function overlaps(a: Item, b: Item, epsMs: number) {
  return a.since < b.until - epsMs && b.since < a.until - epsMs;
}

function trainerKeyOf(it: Item): string {
  const trainers = it.event.instance.trainersList;
  if (!trainers || trainers.length === 0) return '';
  return trainers
    .map((t) => t.personId)
    .toSorted()
    .join(',');
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
  }[] = [];

  // Stable event rank: assigned once on first encounter, never changes.
  // Non-LESSON types get negative ranks so they naturally sort leftward.
  // Trainers get ascending positive ranks in temporal order.
  const eventRank = new Map<string, number>();
  let nextTrainerRank = 0;
  let nextGroupRank = -1;

  const rankKeyOf = (it: Item): string => {
    if (it.event.instance.type !== 'LESSON') {
      return `type:${it.event.event.id}`;
    }
    return trainerKeyOf(it);
  };

  const finalizeGroup = (group: Item[]) => {
    if (group.length === 0) return;

    // Register ranks for new events in temporal order.
    for (const it of group) {
      const key = rankKeyOf(it);
      if (key && !eventRank.has(key)) {
        if (it.event.instance.type !== 'LESSON') {
          eventRank.set(key, nextGroupRank--);
        } else {
          eventRank.set(key, nextTrainerRank++);
        }
      }
    }

    // ---- Phase 1: Lane assignment ----
    // Process in temporal order (required for pack-left correctness: the
    // "check last in lane" optimization only works when events enter each
    // lane in start-time order). Rank is used only as a preference when
    // choosing among free lanes, not as the processing order.
    // Group is already in temporal order from the global sort.

    const lanes: Item[][] = [];

    for (const it of group) {
      const key = rankKeyOf(it);
      const rank = key ? eventRank.get(key) ?? 0 : 0;
      const target = Math.max(0, rank);

      // Find the free lane closest to this event's preferred position.
      let bestLane = -1;
      let bestScore = Infinity;

      for (const [l, lane] of lanes.entries()) {
        const last = lane!.at(-1)!;
        if (last.until > it.since + epsMs) continue; // occupied
        const score = Math.abs(l - target);
        if (score < bestScore) {
          bestScore = score;
          bestLane = l;
        }
      }

      if (bestLane === -1) {
        bestLane = lanes.length;
        lanes.push([]);
      }

      it.lane = bestLane;
      lanes[bestLane]!.push(it);
    }

    const laneCount = Math.max(1, lanes.length);
    const baseWidth = 100 / laneCount;

    // Helper: does item temporally overlap any event assigned to lane l?
    // (Lanes are in temporal order, so we can stop early.)
    const laneAssignedFree = (l: number, it: Item): boolean => {
      for (const other of lanes[l]!) {
        if (other.since >= it.until - epsMs) break;
        if (overlaps(it, other, epsMs)) return false;
      }
      return true;
    };

    // Track expansion claims per lane so two events can't both expand
    // into the same lane during overlapping time spans.
    const claims: { since: number; until: number }[][] = Array.from(
      { length: laneCount },
      () => [],
    );
    for (const it of group) {
      claims[it.lane]!.push({ since: it.since, until: it.until });
    }

    const isClaimed = (l: number, it: Item): boolean => {
      for (const c of claims[l]!) {
        if (it.since < c.until - epsMs && c.since < it.until - epsMs) return true;
      }
      return false;
    };

    const canExpand = (l: number, it: Item): boolean =>
      l >= 0 && l < laneCount && laneAssignedFree(l, it) && !isClaimed(l, it);

    const claimLane = (l: number, it: Item) => {
      claims[l]!.push({ since: it.since, until: it.until });
    };

    // ---- Phase 2: Round-robin expansion ----
    // Instead of letting each event greedily grab all available space,
    // events take turns expanding one lane at a time. This way concurrent
    // events share free space fairly.
    const left = group.map((it) => it.lane);
    const right = group.map((it) => it.lane);
    const expandable = group.map(() => true);

    let changed = true;
    while (changed) {
      changed = false;
      for (const [i, element] of group.entries()) {
        if (!expandable[i]) continue;
        const it = element!;
        let grew = false;

        // Try right first
        if (canExpand(right[i]! + 1, it)) {
          right[i] = right[i]! + 1;
          claimLane(right[i]!, it);
          grew = true;
        }
        // Try left
        if (canExpand(left[i]! - 1, it)) {
          left[i] = left[i]! - 1;
          claimLane(left[i]!, it);
          grew = true;
        }

        if (grew) {
          changed = true;
        } else {
          expandable[i] = false;
        }
      }
    }

    for (const [i, element] of group.entries()) {
      const spanned = right[i]! - left[i]! + 1;
      out.push({
        event: element!.event,
        style: {
          top: element!.top,
          height: element!.height,
          xOffset: left[i]! * baseWidth,
          width: baseWidth * spanned,
        },
      });
    }
  };

  // Partition into overlap-connected groups (rolling max end)
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

  // Render order: top asc, longer events first (shorter rendered later = on top)
  out.sort((a, b) => {
    if (a.style.top !== b.style.top) return a.style.top - b.style.top;
    return b.style.height - a.style.height;
  });

  return out;
}
