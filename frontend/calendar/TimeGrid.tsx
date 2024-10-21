import { cn } from '@/ui/cn';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { eq, inRange } from 'date-arithmetic';
import scrollbarSize from 'dom-helpers/scrollbarSize';
import getWidth from 'dom-helpers/width';
import { useAtomValue } from 'jotai';
import React from 'react';
import DayColumn from './DayColumn';
import TimeGutter from './TimeGutter';
import { diff, format, inEventRange, merge } from './localizer';
import { dragListenersAtom, focusedTimeAtom, maxTimeAtom, minTimeAtom } from './state';
import type { CalendarEvent, Resource } from './types';

interface TimeGridProps {
  events: CalendarEvent[];
  backgroundEvents: CalendarEvent[];
  resources: Resource[];
  range: Date[];
}

const TimeGrid = ({
  events,
  backgroundEvents,
  range,
  resources,
}: TimeGridProps) => {
  const today = new Date();
  const minTime = useAtomValue(minTimeAtom);
  const maxTime = useAtomValue(maxTimeAtom);
  const focusedTime = useAtomValue(focusedTimeAtom);
  const { onDrillDown } = useAtomValue(dragListenersAtom);

  const scrollRef = React.useRef<HTMLDivElement>(null);
  const contentRef = React.useRef<HTMLDivElement>(null);
  const containerRef = React.useRef<HTMLDivElement>(null);
  const gutterRef = React.useRef<HTMLDivElement>(null);

  const [gutterWidth, setGutterWidth] = React.useState<number | undefined>(undefined);
  const [scrollbarMargin, setScrollbarMargin] = React.useState(false);

  useLayoutEffect(() => {
    const width = gutterRef?.current ? getWidth(gutterRef.current) : undefined;
    if (width && gutterWidth !== width) {
      setGutterWidth(width);
    }
    const content = contentRef.current!;
    const isOverflowing = content.scrollHeight > content.clientHeight && content.offsetWidth !== content.clientWidth;
    if (scrollbarMargin !== isOverflowing) {
      setScrollbarMargin(isOverflowing);
    }
  }, [gutterWidth, scrollbarMargin]);

  useLayoutEffect(() => {
    const diffMillis = diff(merge(focusedTime, minTime), focusedTime, 'milliseconds');
    const totalMillis = diff(minTime, maxTime, 'milliseconds');
    const _scrollRatio = diffMillis / totalMillis;
    const content = contentRef.current;
    if (content) {
      content.scrollTop = content.scrollHeight * _scrollRatio;
    }
  }, [focusedTime, minTime, maxTime]);

  const { grouper, groupedEvents, groupedBackgroundEvents } = React.useMemo(() => {
    const dateRange = { start: range[0]!, end: range[range.length - 1]! };
    const rangeEvents = events.filter((event) => inEventRange(event, dateRange));
    const rangeBackgroundEvents = backgroundEvents.filter((event) => inEventRange(event, dateRange));

    const grouper = makeGrouper(resources);
    return {
      grouper,
      groupedEvents: grouper.groupEvents(rangeEvents),
      groupedBackgroundEvents: grouper.groupEvents(rangeBackgroundEvents),
    };
  }, [range, events, backgroundEvents, resources]);

  return (
    <div
      ref={containerRef}
      className={cn('rbc-time-view overscroll-contain', resources && 'rbc-time-view-resources')}
    >
      <div
        ref={scrollRef}
        className="rbc-time-header"
        style={{ marginRight: scrollbarMargin ? `${scrollbarSize() - 1}px` : undefined }}
      >
        <div
          className="px-1 rbc-time-header-gutter"
          style={{ width: gutterWidth, minWidth: gutterWidth, maxWidth: gutterWidth }}
        />

        {grouper.map(([resource], i) => (
          <div className="rbc-time-header-content" key={i + (resource?.resourceId ?? '')}>
            {resource && (
              <div className="rbc-row rbc-row-resource" key={`resource_${resource.resourceId}`}>
                <div className="rbc-header">{resource.resourceTitle}</div>
              </div>
            )}
            <div className={cn('rbc-row', range.length <= 1 && 'hidden')}>
              {range.map((date, i) => (
                <div key={i} className={cn('rbc-header', eq(date, today, 'day') && 'rbc-today')}>
                  <button
                    type="button"
                    className="rbc-button-link"
                    onClick={(e) => {
                      e.preventDefault();
                      onDrillDown(date);
                    }}
                  >
                    <span role="columnheader" aria-sort="none">
                      {format(date, 'dd eee')}
                    </span>
                  </button>
                </div>
              ))}
            </div>
          </div>
        ))}
      </div>

      <div
        ref={contentRef}
        className="rbc-time-content"
        onScroll={(e) => {
          if (scrollRef.current) {
            scrollRef.current.scrollLeft = e.currentTarget.scrollLeft;
          }
        }}
      >
        <TimeGutter className="rbc-time-gutter" gutterRef={gutterRef} date={range[0]!} />

        {grouper.map(([resource, id], i) =>
          range.map((date, jj) => (
            <DayColumn
              gridRef={containerRef}
              resource={resource}
              key={`${i}-${jj}`}
              date={date}
              events={(groupedEvents.get(id) || []).filter((event) =>
                inRange(date, event.start, event.end, 'day'),
              )}
              backgroundEvents={(groupedBackgroundEvents.get(id) || []).filter((event) =>
                inRange(date, event.start, event.end, 'day'),
              )}
            />
          )),
        )}
      </div>
    </div>
  );
};

export default React.memo(TimeGrid);

function makeGrouper(resources: Resource[]) {
  return {
    map<T>(fn: (x: [Resource | undefined, string], ix: number) => T): T[] {
      if (!resources?.length) return [fn([undefined, ''], 0)]
      return resources.map((resource, idx) => fn([resource, resource.resourceId], idx))
    },

    groupEvents(events: CalendarEvent[]): Map<string|undefined, CalendarEvent[]> {
      const eventsByResource = new Map()

      if (!resources?.length) {
        // Return all events if resources are not provided
        eventsByResource.set('', events)
        return eventsByResource
      }

      for (const event of events) {
        for (const id of (event.resourceIds || [''])) {
          if (!resources.find(x => x.resourceId === id)) continue;
          const resourceEvents = eventsByResource.get(id) || []
          resourceEvents.push(event)
          eventsByResource.set(id, resourceEvents)
        }
      }
      return eventsByResource
    },
  }
}
