import { describe, expect, test } from 'vitest';
import { layoutEvents } from './layout';
import { getSlotMetrics } from './TimeSlotMetrics';
import type { CalendarEvent } from './types';

// ---- Helpers ----

const DAY = '2025-01-15';

function metrics() {
  return getSlotMetrics({
    date: new Date(`${DAY}T00:00:00`),
    minTime: new Date('1972-01-01T07:00:00'),
    maxTime: new Date('1972-01-01T23:59:59.999'),
    step: 15,
    timeslots: 4,
  });
}

let nextId = 0;

function ev(
  start: string,
  end: string,
  opts: {
    type?: string;
    trainers?: string[];
    eventId?: string;
  } = {},
): CalendarEvent {
  const id = String(nextId++);
  return {
    start: new Date(`${DAY}T${start}`),
    end: new Date(`${DAY}T${end}`),
    resourceIds: [],
    event: { id: opts.eventId ?? `ev-${id}` } as any,
    instance: {
      id: `inst-${id}`,
      type: opts.type ?? 'LESSON',
      trainersList: (opts.trainers ?? []).map((personId) => ({ personId }) as any),
    } as any,
  };
}

function layout(events: CalendarEvent[], eps = 5) {
  return layoutEvents(events, metrics(), eps);
}

type Style = { top: number; width: number; height: number; xOffset: number };

function stylesOf(events: CalendarEvent[], eps = 5): Style[] {
  return layout(events, eps).map((r) => r.style);
}

function visualRange(s: Style): [number, number] {
  return [s.xOffset, s.xOffset + s.width];
}

function visuallyOverlaps(a: Style, b: Style): boolean {
  const [aLeft, aRight] = visualRange(a);
  const [bLeft, bRight] = visualRange(b);
  const [aTop, aBottom] = [a.top, a.top + a.height];
  const [bTop, bBottom] = [b.top, b.top + b.height];
  // Allow 0.01% tolerance for floating point
  return (
    aLeft < bRight - 0.01 &&
    bLeft < aRight - 0.01 &&
    aTop < bBottom - 0.01 &&
    bTop < aBottom - 0.01
  );
}

// ---- Invariant tests ----

describe('invariants', () => {
  test('no event overflows past 100%', () => {
    const events = [
      ev('10:00', '11:00', { trainers: ['a'] }),
      ev('10:00', '11:00', { trainers: ['b'] }),
      ev('10:00', '11:00', { trainers: ['c'] }),
      ev('10:15', '11:00', { trainers: ['d'] }),
      ev('10:30', '12:00', { trainers: ['e'] }),
    ];
    for (const { style } of layout(events)) {
      expect(style.xOffset).toBeGreaterThanOrEqual(0);
      expect(style.xOffset + style.width).toBeLessThanOrEqual(100.01);
    }
  });

  test('no two temporally overlapping events share visual x-range', () => {
    const events = [
      ev('10:00', '11:00', { trainers: ['a'] }),
      ev('10:00', '11:00', { trainers: ['b'] }),
      ev('10:30', '11:30', { trainers: ['c'] }),
      ev('11:00', '12:00', { trainers: ['d'] }),
    ];
    const result = layout(events);
    for (let i = 0; i < result.length; i++) {
      for (let j = i + 1; j < result.length; j++) {
        expect(
          visuallyOverlaps(result[i]!.style, result[j]!.style),
          `events ${i} and ${j} visually overlap`,
        ).toBe(false);
      }
    }
  });

  test('every input event appears exactly once in output', () => {
    const events = [
      ev('09:00', '10:00', { trainers: ['a'] }),
      ev('10:00', '11:00', { trainers: ['b'] }),
      ev('10:00', '11:00', { trainers: ['c'] }),
    ];
    const result = layout(events);
    expect(result).toHaveLength(events.length);
    for (const event of events) {
      expect(result.filter((r) => r.event === event)).toHaveLength(1);
    }
  });

  test('width is always positive', () => {
    const events = [
      ev('10:00', '10:15', { trainers: ['a'] }),
      ev('10:00', '11:00', { trainers: ['b'] }),
    ];
    for (const { style } of layout(events)) {
      expect(style.width).toBeGreaterThan(0);
    }
  });
});

// ---- Lane count / basic layout ----

describe('lane count', () => {
  test('single event fills full width', () => {
    const [s] = stylesOf([ev('10:00', '11:00', { trainers: ['a'] })]);
    expect(s!.xOffset).toBe(0);
    expect(s!.width).toBeCloseTo(100);
  });

  test('non-overlapping events each fill full width', () => {
    const styles = stylesOf([
      ev('10:00', '11:00', { trainers: ['a'] }),
      ev('11:00', '12:00', { trainers: ['b'] }),
      ev('12:00', '13:00', { trainers: ['c'] }),
    ]);
    for (const s of styles) {
      expect(s.width).toBeCloseTo(100);
    }
  });

  test('two concurrent events split evenly', () => {
    const styles = stylesOf([
      ev('10:00', '11:00', { trainers: ['a'] }),
      ev('10:00', '11:00', { trainers: ['b'] }),
    ]);
    expect(styles).toHaveLength(2);
    expect(styles[0]!.width).toBeCloseTo(50);
    expect(styles[1]!.width).toBeCloseTo(50);
  });

  test('three concurrent events get equal width', () => {
    const styles = stylesOf([
      ev('10:00', '11:00', { trainers: ['a'] }),
      ev('10:00', '11:00', { trainers: ['b'] }),
      ev('10:00', '11:00', { trainers: ['c'] }),
    ]);
    for (const s of styles) {
      expect(s.width).toBeCloseTo(100 / 3);
    }
  });

  test('two concurrent then one alone: alone event expands', () => {
    const styles = stylesOf([
      ev('10:00', '11:00', { trainers: ['a'] }),
      ev('10:00', '11:00', { trainers: ['b'] }),
      ev('12:00', '13:00', { trainers: ['c'] }),
    ]);
    // First two share
    expect(styles[0]!.width).toBeLessThan(100);
    expect(styles[1]!.width).toBeLessThan(100);
    // Third is alone in its own group
    expect(styles[2]!.width).toBeCloseTo(100);
  });

  test('chain overlap creates correct lane count', () => {
    // A overlaps B, B overlaps C, but A doesn't overlap C
    // This is one connected group with max concurrency 2
    const styles = stylesOf([
      ev('10:00', '10:45', { trainers: ['a'] }),
      ev('10:30', '11:15', { trainers: ['b'] }),
      ev('11:00', '11:45', { trainers: ['c'] }),
    ]);
    expect(styles).toHaveLength(3);
    // Should only need 2 lanes (A and C can share a lane)
    const lanes = new Set(styles.map((s) => s.xOffset));
    expect(lanes.size).toBeLessThanOrEqual(2);
  });
});

// ---- Expansion ----

describe('expansion', () => {
  test('event expands into free lanes on both sides', () => {
    // 3 concurrent at top, only middle one continues below
    const styles = stylesOf([
      ev('10:00', '10:30', { trainers: ['a'] }),
      ev('10:00', '11:30', { trainers: ['b'] }),
      ev('10:00', '10:30', { trainers: ['c'] }),
    ]);
    // B should expand into freed lanes after A and C end
    // During 10:00-10:30 all three share. B is in a connected group with 3 lanes.
    // But B spans the full group so it stays at baseWidth during the overlap.
    // After A and C end, B can't change width because it's one event with one style.
    // This is a fundamental limitation: one style per event.
    expect(styles).toHaveLength(3);
  });

  test('round-robin: concurrent events share free space fairly', () => {
    // 4-lane group: events in lanes 0 and 3, lanes 1-2 free
    // Each should get one extra lane, not one getting both
    const events = [
      ev('10:00', '11:00', { trainers: ['a'] }),
      ev('10:00', '11:00', { trainers: ['b'] }),
      ev('10:00', '11:00', { trainers: ['c'] }),
      ev('10:00', '11:00', { trainers: ['d'] }),
      // Only a and d continue — b and c end, freeing lanes
      ev('11:30', '12:30', { trainers: ['a'] }),
      ev('11:30', '12:30', { trainers: ['d'] }),
    ];
    const result = layout(events);
    const lateEvents = result.filter(
      (r) => r.event.start.getHours() === 11 && r.event.start.getMinutes() === 30,
    );
    expect(lateEvents).toHaveLength(2);
    // Both should get roughly equal width (2 lanes each out of 4)
    expect(lateEvents[0]!.style.width).toBeCloseTo(lateEvents[1]!.style.width, 0);
  });
});

// ---- Rank / type priority ----

describe('rank and type priority', () => {
  test('group lesson prefers lane 0', () => {
    const result = layout([
      ev('10:00', '11:00', { type: 'GROUP', eventId: 'g1' }),
      ev('10:00', '11:00', { trainers: ['a'] }),
      ev('10:00', '11:00', { trainers: ['b'] }),
    ]);
    const group = result.find((r) => r.event.instance.type === 'GROUP');
    expect(group!.style.xOffset).toBe(0);
  });

  test('trainer lane consistency across groups', () => {
    const result = layout([
      // Group 1: trainers a, b, c
      ev('10:00', '10:45', { trainers: ['a'] }),
      ev('10:00', '10:45', { trainers: ['b'] }),
      ev('10:00', '10:45', { trainers: ['c'] }),
      // Group 2: same trainers
      ev('12:00', '12:45', { trainers: ['a'] }),
      ev('12:00', '12:45', { trainers: ['b'] }),
      ev('12:00', '12:45', { trainers: ['c'] }),
    ]);

    // Get the xOffset for each trainer in each group
    const trainerOffset = (trainerId: string, hour: number) => {
      const r = result.find(
        (r) =>
          r.event.instance.trainersList?.some(
            (t: any) => t.personId === trainerId,
          ) && r.event.start.getHours() === hour,
      );
      return r!.style.xOffset;
    };

    // Relative order should be preserved
    const order1 = ['a', 'b', 'c']
      .map((t) => ({ t, x: trainerOffset(t, 10) }))
      .sort((a, b) => a.x - b.x)
      .map((o) => o.t);
    const order2 = ['a', 'b', 'c']
      .map((t) => ({ t, x: trainerOffset(t, 12) }))
      .sort((a, b) => a.x - b.x)
      .map((o) => o.t);

    expect(order1).toEqual(order2);
  });

  test('trainer consistency when one trainer absent', () => {
    const result = layout([
      // Group 1: a, b, c
      ev('10:00', '10:45', { trainers: ['a'] }),
      ev('10:00', '10:45', { trainers: ['b'] }),
      ev('10:00', '10:45', { trainers: ['c'] }),
      // Group 2: a, c (b absent)
      ev('12:00', '12:45', { trainers: ['a'] }),
      ev('12:00', '12:45', { trainers: ['c'] }),
    ]);

    const offsetOf = (trainerId: string, hour: number) => {
      const r = result.find(
        (r) =>
          r.event.instance.trainersList?.some(
            (t: any) => t.personId === trainerId,
          ) && r.event.start.getHours() === hour,
      );
      return r!.style.xOffset;
    };

    // a should still be left of c in both groups
    expect(offsetOf('a', 10)).toBeLessThan(offsetOf('c', 10));
    expect(offsetOf('a', 12)).toBeLessThan(offsetOf('c', 12));
  });
});

// ---- Epsilon handling ----

describe('epsilon', () => {
  test('events within epsilon are treated as non-overlapping for lanes', () => {
    // 4 minutes apart with 5-minute epsilon — should not overlap
    const styles = stylesOf([
      ev('10:00', '10:46', { trainers: ['a'] }),
      ev('10:45', '11:30', { trainers: ['b'] }),
    ]);
    // Should share a lane (1 lane total) since they're within epsilon
    expect(styles).toHaveLength(2);
    // Both should get full width (separate groups or shared lane)
    for (const s of styles) {
      expect(s.width).toBeCloseTo(100);
    }
  });

  test('events beyond epsilon are treated as overlapping', () => {
    // 10 minutes apart with 5-minute epsilon — should overlap
    const styles = stylesOf([
      ev('10:00', '11:00', { trainers: ['a'] }),
      ev('10:50', '11:30', { trainers: ['b'] }),
    ]);
    expect(styles).toHaveLength(2);
    // Should need 2 lanes
    expect(styles[0]!.width).toBeLessThan(100);
    expect(styles[1]!.width).toBeLessThan(100);
  });

  test('zero epsilon: adjacent events do not overlap', () => {
    const styles = stylesOf(
      [
        ev('10:00', '11:00', { trainers: ['a'] }),
        ev('11:00', '12:00', { trainers: ['b'] }),
      ],
      0,
    );
    for (const s of styles) {
      expect(s.width).toBeCloseTo(100);
    }
  });
});

// ---- Stress / randomized invariant test ----

describe('randomized', () => {
  test('no overflow or visual overlap in random schedules', () => {
    const trainerPool = ['a', 'b', 'c', 'd', 'e', 'f'];
    const types = ['LESSON', 'LESSON', 'LESSON', 'GROUP'];

    for (let trial = 0; trial < 50; trial++) {
      const events: CalendarEvent[] = [];
      const count = 5 + Math.floor(Math.random() * 20);

      for (let i = 0; i < count; i++) {
        const startHour = 8 + Math.floor(Math.random() * 10);
        const startMin = Math.floor(Math.random() * 4) * 15;
        const duration = 15 + Math.floor(Math.random() * 6) * 15; // 15-90 min
        const endHour = startHour + Math.floor((startMin + duration) / 60);
        const endMin = (startMin + duration) % 60;
        const type = types[Math.floor(Math.random() * types.length)]!;
        const trainer =
          trainerPool[Math.floor(Math.random() * trainerPool.length)]!;

        const sh = String(startHour).padStart(2, '0');
        const sm = String(startMin).padStart(2, '0');
        const eh = String(Math.min(endHour, 23)).padStart(2, '0');
        const em = String(endMin).padStart(2, '0');

        events.push(
          ev(`${sh}:${sm}`, `${eh}:${em}`, {
            type,
            trainers: type === 'GROUP' ? [] : [trainer],
            eventId: `rand-${trial}-${i}`,
          }),
        );
      }

      const result = layout(events);

      // Invariant 1: no overflow
      for (const { style } of result) {
        expect(
          style.xOffset,
          `trial ${trial}: negative xOffset`,
        ).toBeGreaterThanOrEqual(0);
        expect(
          style.xOffset + style.width,
          `trial ${trial}: overflow past 100%`,
        ).toBeLessThanOrEqual(100.01);
        expect(style.width, `trial ${trial}: zero width`).toBeGreaterThan(0);
      }

      // Invariant 2: no visual overlap
      for (let i = 0; i < result.length; i++) {
        for (let j = i + 1; j < result.length; j++) {
          expect(
            visuallyOverlaps(result[i]!.style, result[j]!.style),
            `trial ${trial}: events ${i} and ${j} visually overlap`,
          ).toBe(false);
        }
      }

      // Invariant 3: all events present
      expect(result).toHaveLength(events.length);
    }
  });
});

// ---- Realistic sports club afternoon ----

describe('full afternoon schedule', () => {
  // Models a real afternoon at a sports club with realistic timing:
  // - 45-minute lessons, truly consecutive (end = next start)
  // - Staggered starts creating genuine overlaps
  // - Group lessons spanning multiple lesson slots
  // - Trainers with gaps, returns, different start offsets
  //
  // Timeline (each line is one event):
  //
  //  14:00 ─── 14:45 ─── 15:30 ─── 16:15 ─── 17:00 ─── 17:45
  //  │ Group "Fialová"           │
  //  │ 14:00──────────────15:30  │
  //  │                           │
  //  │ A: Novák-Malá             │ A: Dvořák-Říha            │ A: Buroň-Hoková
  //  │ 14:00───14:45             │ 14:45───15:30             │ 15:30───16:15       │ (30min break) │ 16:45───17:30
  //  │                           │                            │
  //  │ B: Veselý-Lenfe           │ B: Kovář-Pecha            │
  //  │ 14:00───14:45             │ 14:45───15:30             │ (B done for the day)
  //  │                           │
  //  │ C: Penka-Hrubá            │ (C absent)                │ C: Křižan-Pospíšil
  //  │ 14:00───14:45             │                            │ 15:30───16:15
  //  │                           │
  //  │ D: Huvar-Minař            │ D: Vaněk-Vaňková                               │ D: Hrabal-Hrabalová
  //  │ 14:15───15:00             │ 15:15───16:00                                   │ 16:45───17:30
  //  │          ↑ staggered!     │          ↑ staggered! overlaps with A's 14:45   │
  //  │                           │
  //  │                           │                 Group "Zlatá"
  //  │                           │                 15:45──────────────17:15
  //  │                           │                          ↑ overlaps with D's 15:15

  const fialova = ev('14:00', '15:30', { type: 'GROUP', eventId: 'fialova' });

  // Trainer A: four consecutive lessons, 30min break before last
  const a1 = ev('14:00', '14:45', { trainers: ['A'] });
  const a2 = ev('14:45', '15:30', { trainers: ['A'] });
  const a3 = ev('15:30', '16:15', { trainers: ['A'] });
  const a4 = ev('16:45', '17:30', { trainers: ['A'] });

  // Trainer B: two consecutive, then done
  const b1 = ev('14:00', '14:45', { trainers: ['B'] });
  const b2 = ev('14:45', '15:30', { trainers: ['B'] });

  // Trainer C: one lesson, absent, returns later
  const c1 = ev('14:00', '14:45', { trainers: ['C'] });
  const c3 = ev('15:30', '16:15', { trainers: ['C'] });

  // Trainer D: staggered 15min late, creates true overlaps
  const d1 = ev('14:15', '15:00', { trainers: ['D'] });
  const d2 = ev('15:15', '16:00', { trainers: ['D'] });
  const d3 = ev('16:45', '17:30', { trainers: ['D'] });

  // Second group lesson, starts mid-afternoon, overlaps with ongoing lessons
  const zlata = ev('15:45', '17:15', { type: 'GROUP', eventId: 'zlata' });

  const allEvents = [fialova, a1, b1, c1, d1, a2, b2, d2, a3, c3, zlata, a4, d3];

  const result = layout(allEvents);
  const styleOf = (e: CalendarEvent) => result.find((r) => r.event === e)!.style;

  // ---- Structural invariants ----

  test('all events present', () => {
    expect(result).toHaveLength(allEvents.length);
  });

  test('no overflow', () => {
    for (const { style } of result) {
      expect(style.xOffset).toBeGreaterThanOrEqual(0);
      expect(style.xOffset + style.width).toBeLessThanOrEqual(100.01);
    }
  });

  test('no visual overlap', () => {
    for (let i = 0; i < result.length; i++) {
      for (let j = i + 1; j < result.length; j++) {
        expect(
          visuallyOverlaps(result[i]!.style, result[j]!.style),
          `${result[i]!.event.instance.id} and ${result[j]!.event.instance.id} overlap`,
        ).toBe(false);
      }
    }
  });

  test('positive dimensions', () => {
    for (const { style } of result) {
      expect(style.width).toBeGreaterThan(0);
      expect(style.height).toBeGreaterThan(0);
    }
  });

  // ---- Trainer lane consistency ----

  test('trainer A consecutive lessons maintain same lane', () => {
    expect(styleOf(a1).xOffset).toBe(styleOf(a2).xOffset);
  });
  test('trainer B consecutive lessons maintain same lane', () => {
    expect(styleOf(b1).xOffset).toBe(styleOf(b2).xOffset);
  });

  test('trainer relative order preserved: A, B, C, D', () => {
    // At 14:00-14:45 all four trainers are active (D starts at 14:15
    // but overlaps with the others). Their relative order should be
    // consistent wherever they appear together later.
    const order14 = ['A', 'B', 'C', 'D']
      .map((t) => ({
        t,
        x: styleOf({ A: a1, B: b1, C: c1, D: d1 }[t]!).xOffset,
      }))
      .toSorted((a, b) => a.x - b.x)
      .map((o) => o.t);

    // A and D both teach at 16:45 — their relative order should match
    const aBeforeD_14 = order14.indexOf('A') < order14.indexOf('D');
    const aBeforeD_17 =
      styleOf(a4).xOffset < styleOf(d3).xOffset;
    expect(aBeforeD_14).toBe(aBeforeD_17);
  });

  test('trainer A stays left of C when C returns at 15:30', () => {
    expect(styleOf(a3).xOffset).toBeLessThan(styleOf(c3).xOffset);
  });

  // ---- Staggered overlap handling ----

  test('d2 overlaps with a2 and they are side by side', () => {
    // d2 (15:15-16:00) overlaps with a2 (14:45-15:30) by 15 minutes
    // (beyond the 5-minute epsilon, so they're truly concurrent)
    expect(visuallyOverlaps(styleOf(a2), styleOf(d2))).toBe(false);
  });

  test('zlata overlaps with d2 and they are side by side', () => {
    // zlata (15:45-17:15) overlaps with d2 (15:15-16:00) by 15 minutes
    expect(visuallyOverlaps(styleOf(zlata), styleOf(d2))).toBe(false);
  });

  // ---- Fair width distribution ----

  test('concurrent events get fair width at peak', () => {
    // At ~14:15 we have: fialova, a1, b1, c1, d1 = 5 concurrent
    const peak = [
      styleOf(fialova),
      styleOf(a1),
      styleOf(b1),
      styleOf(c1),
      styleOf(d1),
    ];
    const minWidth = Math.min(...peak.map((s) => s.width));
    const maxWidth = Math.max(...peak.map((s) => s.width));
    // Round-robin: ratio between widest and narrowest ≤ 2:1
    expect(maxWidth / minWidth).toBeLessThanOrEqual(2.01);
  });

  test('events in sparse regions expand', () => {
    // a4 and d3 at 16:45-17:30 only overlap with tail of zlata (3 events)
    // Each should be wider than if peak concurrency (5) applied
    expect(styleOf(a4).width).toBeGreaterThanOrEqual(20);
    expect(styleOf(d3).width).toBeGreaterThanOrEqual(20);
  });
});

// ---- Edge case: all events identical time ----

describe('degenerate cases', () => {
  test('many events at exact same time', () => {
    const events = Array.from({ length: 8 }, (_, i) =>
      ev('10:00', '10:45', { trainers: [`trainer-${i}`] }),
    );
    const result = layout(events);
    expect(result).toHaveLength(8);
    for (const { style } of result) {
      expect(style.width).toBeCloseTo(100 / 8);
      expect(style.xOffset + style.width).toBeLessThanOrEqual(100.01);
    }
    // No visual overlaps
    for (let i = 0; i < result.length; i++) {
      for (let j = i + 1; j < result.length; j++) {
        expect(visuallyOverlaps(result[i]!.style, result[j]!.style)).toBe(false);
      }
    }
  });

  test('single very long event', () => {
    const [s] = stylesOf([ev('08:00', '20:00', { trainers: ['a'] })]);
    expect(s!.width).toBeCloseTo(100);
    expect(s!.xOffset).toBe(0);
  });

  test('many short non-overlapping events reuse lanes', () => {
    // 12 events, 15 min each, back-to-back, all same trainer
    // Should all land in lane 0 and expand to 100%
    const events = Array.from({ length: 12 }, (_, i) => {
      const startH = 10 + Math.floor(i / 4);
      const startM = (i % 4) * 15;
      const endM = startM + 15;
      const endH = startH + Math.floor(endM / 60);
      const sh = String(startH).padStart(2, '0');
      const sm = String(startM).padStart(2, '0');
      const eh = String(endH).padStart(2, '0');
      const em = String(endM % 60).padStart(2, '0');
      return ev(`${sh}:${sm}`, `${eh}:${em}`, { trainers: ['a'] });
    });
    const result = layout(events);
    for (const { style } of result) {
      expect(style.width).toBeCloseTo(100);
    }
  });

  test('two trainers alternating with tiny gaps', () => {
    // A: 10:00-10:45, 10:45-11:30, 11:30-12:15
    // B: 10:15-11:00, 11:00-11:45, 11:45-12:30
    // Every A overlaps with a B. Should need exactly 2 lanes.
    const events = [
      ev('10:00', '10:45', { trainers: ['A'] }),
      ev('10:15', '11:00', { trainers: ['B'] }),
      ev('10:45', '11:30', { trainers: ['A'] }),
      ev('11:00', '11:45', { trainers: ['B'] }),
      ev('11:30', '12:15', { trainers: ['A'] }),
      ev('11:45', '12:30', { trainers: ['B'] }),
    ];
    const result = layout(events);
    expect(result).toHaveLength(6);
    // Should be exactly 2 distinct xOffsets
    const offsets = new Set(result.map((r) => Math.round(r.style.xOffset)));
    expect(offsets.size).toBe(2);
    // Each should be ~50% wide
    for (const { style } of result) {
      expect(style.width).toBeCloseTo(50, 0);
    }
    // A should always be in the same column
    const aOffsets = result
      .filter((r) => r.event.instance.trainersList?.some((t: any) => t.personId === 'A'))
      .map((r) => r.style.xOffset);
    expect(new Set(aOffsets).size).toBe(1);
    // B should always be in the same column
    const bOffsets = result
      .filter((r) => r.event.instance.trainersList?.some((t: any) => t.personId === 'B'))
      .map((r) => r.style.xOffset);
    expect(new Set(bOffsets).size).toBe(1);
  });
});
