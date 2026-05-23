import { cn } from '@/lib/cn';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { add, eq, startOf } from 'date-arithmetic';
import { useAtomValue } from 'jotai';
import React from 'react';
import DayColumn from './DayColumn';
import TimeGutter from './TimeGutter';
import { diff, format, inEventRange, merge, range } from './localizer';
import { dragListenersAtom, focusedTimeAtom, maxTimeAtom, minTimeAtom } from './state';
import type { CalendarEvent, DateRange, Resource } from './types';

type PrimaryGrouping = 'resource' | 'day';

interface TimeGridProps {
  events: readonly CalendarEvent[];
  backgroundEvents: readonly CalendarEvent[];
  resources: readonly Resource[];
  range: DateRange;
  primary?: PrimaryGrouping;
}

type Grid = {
  days: readonly Date[];
  resources: readonly (Resource | undefined)[];
  events: CalendarEvent[][][]; // [r][d]
  bg: CalendarEvent[][][]; // [r][d]
};

export default React.memo(function TimeGrid({
  events,
  backgroundEvents,
  range,
  resources,
  primary = 'resource',
}: TimeGridProps) {
  const today = new Date();
  const minTime = useAtomValue(minTimeAtom);
  const maxTime = useAtomValue(maxTimeAtom);
  const focusedTime = useAtomValue(focusedTimeAtom);

  const scrollRef = React.useRef<HTMLDivElement>(null);
  const contentRef = React.useRef<HTMLDivElement>(null);
  const containerRef = React.useRef<HTMLDivElement>(null);
  const gutterRef = React.useRef<HTMLDivElement>(null);

  const [gutterWidth, setGutterWidth] = React.useState<number | undefined>();

  useLayoutEffect(() => {
    if (!gutterRef?.current) return;
    const width = gutterRef.current.getBoundingClientRect().width;
    if (width && gutterWidth !== width) {
      setGutterWidth(width);
    }
  }, [gutterWidth]);

  useLayoutEffect(() => {
    const content = contentRef.current;
    if (!content) return;
    const totalMillis = diff(minTime, maxTime, 'milliseconds');
    if (totalMillis <= 0) return;

    const diffMillis = diff(merge(focusedTime, minTime), focusedTime, 'milliseconds');
    const ratio = Math.min(1, Math.max(0, diffMillis / totalMillis));
    content.scrollTop = (content.scrollHeight - content.clientHeight) * ratio;
  }, [focusedTime, minTime, maxTime]);

  const grid = React.useMemo(
    () => buildGrid(range, resources, events, backgroundEvents),
    [range, resources, events, backgroundEvents],
  );

  return (
    <div
      ref={containerRef}
      className={cn(
        'rbc-time-view overscroll-contain',
        resources.length > 0 && 'rbc-time-view-resources',
      )}
    >
      <div ref={scrollRef} className="rbc-time-header">
        <div
          className="px-1 rbc-time-header-gutter"
          style={{ width: gutterWidth, minWidth: gutterWidth, maxWidth: gutterWidth }}
        />
        {primary === 'resource'
          ? grid.resources.map((resource) => (
              <div
                className="rbc-time-header-content"
                key={resource?.resourceId ?? '__nothing__'}
              >
                {resource && (
                  <div className="rbc-row">
                    <div className="rbc-header">{resource.resourceTitle}</div>
                  </div>
                )}
                <div className={cn('rbc-row', grid.days.length <= 1 && 'hidden')}>
                  {grid.days.map((date) => (
                    <DayButton key={+date} today={today} date={date} />
                  ))}
                </div>
              </div>
            ))
          : grid.days.map((date) => (
              <div className="rbc-time-header-content" key={+date}>
                <div className="rbc-row">
                  <DayButton today={today} date={date} />
                </div>
                <div className={cn('rbc-row', grid.resources.length <= 1 && 'hidden')}>
                  {grid.resources.map((resource) => (
                    <div
                      key={resource?.resourceId ?? '__nothing__'}
                      className="rbc-header"
                    >
                      {resource?.resourceTitle ?? ''}
                    </div>
                  ))}
                </div>
              </div>
            ))}
      </div>

      <div
        ref={contentRef}
        className="rbc-time-content"
        style={{
          WebkitTouchCallout: 'none',
          WebkitUserSelect: 'none',
          userSelect: 'none',
        }}
        onSelectCapture={(e) => e.preventDefault()}
        onScroll={(e) => {
          if (scrollRef.current) {
            scrollRef.current.scrollLeft = e.currentTarget.scrollLeft;
          }
        }}
      >
        <TimeGutter gutterRef={gutterRef} date={grid.days[0]!} />

        {primary === 'resource'
          ? grid.resources.flatMap((resource, rIdx) =>
              grid.days.map((date, dIdx) => (
                <DayColumn
                  gridRef={containerRef}
                  resource={resource}
                  key={`${resource?.resourceId ?? '__nothing__'}-${+date}`}
                  date={date}
                  events={grid.events[rIdx]![dIdx]!}
                  backgroundEvents={grid.bg[rIdx]![dIdx]!}
                />
              )),
            )
          : grid.days.flatMap((date, dIdx) =>
              grid.resources.map((resource, rIdx) => (
                <DayColumn
                  gridRef={containerRef}
                  resource={resource}
                  key={`${+date}-${resource?.resourceId ?? '__nothing__'}`}
                  date={date}
                  events={grid.events[rIdx]![dIdx]!}
                  backgroundEvents={grid.bg[rIdx]![dIdx]!}
                />
              )),
            )}
      </div>
    </div>
  );
});

function buildGrid(
  { since, until }: DateRange,
  resources: readonly Resource[],
  events: readonly CalendarEvent[],
  bgEvents: readonly CalendarEvent[],
): Grid {
  const days = range(since, until, 'day').map((d) => startOf(d, 'day'));
  const viewStart = days[0]!;
  const viewEnd = days.at(-1)!;

  const hasResources = resources.length > 0;
  const resourceList: (Resource | undefined)[] = hasResources
    ? [...resources]
    : [undefined];

  const resourceIdx = new Map<string, number>();
  if (hasResources)
    for (let i = 0; i < resources.length; i++)
      resourceIdx.set(resources[i]!.resourceId, i);

  const dayIdx = new Map<number, number>();
  for (let i = 0; i < days.length; i++) dayIdx.set(+days[i]!, i);

  const init = () =>
    Array.from({ length: resourceList.length }, () =>
      Array.from({ length: days.length }, () => [] as CalendarEvent[]),
    );

  const gridEvents = init();
  const gridBg = init();

  const dateRange = { start: viewStart, end: viewEnd };

  const distribute = (src: readonly CalendarEvent[], out: CalendarEvent[][][]) => {
    for (const ev of src) {
      if (!inEventRange(ev, dateRange)) continue;

      const evStart = startOf(new Date(Math.max(+ev.start, +viewStart)), 'day');
      const evEnd = startOf(new Date(Math.min(+ev.end, +viewEnd)), 'day');

      const ids = hasResources ? ev.resourceIds : ['__nothing__'];
      for (const rid of ids) {
        const r = hasResources ? resourceIdx.get(rid) : 0;
        if (r === undefined) continue;

        for (let d = evStart; +d <= +evEnd; d = add(d, 1, 'day')) {
          const di = dayIdx.get(+d);
          if (di === undefined) continue;
          out[r]![di]!.push(ev);
        }
      }
    }
  };

  distribute(events, gridEvents);
  distribute(bgEvents, gridBg);

  return { days, resources: resourceList, events: gridEvents, bg: gridBg };
}

function DayButton({ today, date }: { today: Date; date: Date }) {
  const { onDrillDown } = useAtomValue(dragListenersAtom);
  return (
    <div
      className={cn('rbc-header', {
        'bg-accent-3/80': eq(date, today, 'day'),
      })}
    >
      <button
        type="button"
        className="text-inherit bg-transparent m-0 p-0 border-none cursor-pointer select-text"
        onClick={(e) => {
          e.preventDefault();
          onDrillDown?.(date);
        }}
      >
        <span role="columnheader" aria-sort="none">
          {format(date, 'dd eee')}
        </span>
      </button>
    </div>
  );
}
