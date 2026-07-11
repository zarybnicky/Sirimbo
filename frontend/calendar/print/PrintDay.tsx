import React from 'react';
import { add, startOf } from 'date-arithmetic';
import { ceil, format, shortTimeIntl } from '@/calendar/localizer';
import { capitalize, formatDefaultInstanceName } from '@/ui/format';
import { getSlotMetrics } from '@/calendar/TimeSlotMetrics';
import { layoutEvents } from '@/calendar/layout';
import type { CalendarEvent, CalendarInstanceEvent, Resource } from '@/calendar/types';
import { cn } from '@/lib/cn';

/** Vertical scale of the printed grid. */
const PX_PER_HOUR = 52;

type Column = {
  key: string;
  title: string;
  events: CalendarInstanceEvent[];
};

/**
 * Print-ready day. Reuses the calendar's layout engine (`getSlotMetrics` +
 * `layoutEvents`) for proportional placement and overlap packing, but renders
 * its own paper-optimized markup instead of the interactive time-grid. The
 * time window is trimmed to the day's actual events, group lessons get their
 * own leading column, and trainers follow.
 */
export function PrintDay({
  date,
  events,
  resources,
}: {
  date: Date;
  events: readonly CalendarEvent[];
  resources: readonly Resource[];
}) {
  const { columns, minTime, maxTime, isEmpty } = React.useMemo(
    () => planDay(events, date, resources),
    [events, date, resources],
  );

  const metrics = React.useMemo(
    () => getSlotMetrics({ date, minTime, maxTime, step: 30, timeslots: 2 }),
    [date, minTime, maxTime],
  );

  const hourTops = React.useMemo(
    () => metrics.groups.map((g) => ({ time: g[0]!, top: metrics.getRange(g[0]!, g[0]!).top })),
    [metrics],
  );
  const bodyHeight = Math.max(1, metrics.groups.length) * PX_PER_HOUR;

  return (
    <section className="print-day">
      <h2 className="mb-1 text-lg font-semibold break-after-avoid">
        {capitalize(format(date, 'EEEE d. MMMM yyyy'))}
      </h2>

      {isEmpty ? (
        <p className="text-sm text-neutral-11">Žádné tréninky</p>
      ) : (
        <div className="flex text-[10px] leading-tight">
          <TimeGutter hourTops={hourTops} height={bodyHeight} />
          {columns.map((col) => (
            <div key={col.key} className="min-w-0 flex-1 border-l border-neutral-8">
              <div className="truncate border-b border-neutral-8 bg-neutral-3 px-1 py-0.5 text-center text-[11px] font-semibold">
                {col.title}
              </div>
              <div className="relative" style={{ height: bodyHeight }}>
                {hourTops.map((h) => (
                  <div
                    key={+h.time}
                    className="absolute inset-x-0 border-t border-neutral-4"
                    style={{ top: `${h.top}%` }}
                  />
                ))}
                {layoutEvents([...col.events], metrics, 5).map(({ event, style }) => (
                  <EventBlock
                    key={(event as CalendarInstanceEvent).instance.id}
                    event={event as CalendarInstanceEvent}
                    style={style}
                  />
                ))}
              </div>
            </div>
          ))}
        </div>
      )}
    </section>
  );
}

function TimeGutter({
  hourTops,
  height,
}: {
  hourTops: { time: Date; top: number }[];
  height: number;
}) {
  return (
    <div className="w-9 shrink-0">
      <div className="border-b border-transparent px-1 py-0.5 text-[11px]">&nbsp;</div>
      <div className="relative tabular-nums text-neutral-11" style={{ height }}>
        {hourTops.map((h) => (
          <div
            key={+h.time}
            className="absolute right-1 -translate-y-1/2"
            style={{ top: `${h.top}%` }}
          >
            {format(h.time, 'HH')}
          </div>
        ))}
      </div>
    </div>
  );
}

function EventBlock({
  event,
  style,
}: {
  event: CalendarInstanceEvent;
  style: { top: number; height: number; width: number; xOffset: number };
}) {
  const { instance } = event;
  const isGroup = instance.type !== 'LESSON';
  const color = isGroup
    ? instance.targetCohortsList?.find((x) => x.cohort?.colorRgb)?.cohort?.colorRgb
    : undefined;
  const free = !isGroup && instance.registrations.totalCount === 0;

  return (
    <div
      style={{
        top: `${style.top}%`,
        height: `${style.height}%`,
        left: `${style.xOffset}%`,
        width: `${style.width}%`,
        backgroundColor: color,
      }}
      className={cn(
        'absolute overflow-hidden rounded-[2px] border border-neutral-8 px-1',
        isGroup ? 'font-medium' : 'bg-neutral-0',
        isGroup && !color && 'bg-neutral-4',
        free && 'print-hatch text-neutral-11',
      )}
    >
      <span className="mr-1 tabular-nums text-neutral-11">
        {shortTimeIntl.format(event.start)}
      </span>
      <span className={cn('break-words', instance.isCancelled && 'line-through')}>
        {instance.name || formatDefaultInstanceName(instance)}
      </span>
    </div>
  );
}

/** Splits a day's events into a leading group column plus one column per
 * trainer, and trims the visible window to the events that exist. */
function planDay(
  events: readonly CalendarEvent[],
  date: Date,
  resources: readonly Resource[],
): { columns: Column[]; minTime: Date; maxTime: Date; isEmpty: boolean } {
  const dayEvents = events
    .filter((e): e is CalendarInstanceEvent => e.kind === 'event')
    .filter((e) => +startOf(e.start, 'day') === +startOf(date, 'day'))
    .toSorted((a, b) => +a.start - +b.start);

  const groups = dayEvents.filter((e) => e.instance.type !== 'LESSON');
  const lessons = dayEvents.filter((e) => e.instance.type === 'LESSON');
  const present = new Set(lessons.flatMap((e) => e.resourceIds));

  const columns: Column[] = [];
  if (groups.length > 0) {
    columns.push({ key: '__groups__', title: 'Společné', events: groups });
  }
  for (const r of resources) {
    if (!present.has(r.resourceId)) continue;
    columns.push({
      key: r.resourceId,
      title: r.resourceTitle,
      events: lessons.filter((e) => e.resourceIds.includes(r.resourceId)),
    });
  }

  const dayStart = startOf(date, 'day');
  if (dayEvents.length === 0) {
    return {
      columns,
      minTime: add(dayStart, 8, 'hours'),
      maxTime: add(dayStart, 20, 'hours'),
      isEmpty: true,
    };
  }

  const startMs = Math.min(...dayEvents.map((e) => +e.start));
  const endMs = Math.max(...dayEvents.map((e) => +e.end));
  return {
    columns,
    minTime: startOf(new Date(startMs), 'hours'),
    maxTime: ceil(new Date(endMs), 'hours'),
    isEmpty: false,
  };
}
