import React from 'react';
import clsx from 'clsx';
import DayColumn from './DayColumn';
import TimeGutter from './TimeGutter';
import TimeGridHeader from './TimeGridHeader';
import getWidth from 'dom-helpers/width';
import {
  eq,
  sortEvents,
  inEventRange,
  merge,
  inRange,
  startAndEndAreDateOnly,
  diff,
} from './localizer';
import makeGrouper from './ResourceGrouper';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { Event, Resource } from './types';
import { NavigationContext } from 'NavigationContext';

interface TimeGridProps {
  events: Event[];
  backgroundEvents: Event[];
  resources: Resource[];
  range: Date[];
}

const TimeGrid = ({
  events,
  backgroundEvents,
  range,
  resources,
}: TimeGridProps) => {
  const dateRange = { start: range[0]!, end: range[range.length - 1]! };
  const { min, max, focusedTime } = React.useContext(NavigationContext);

  const scrollRef = React.useRef<HTMLDivElement>(null);
  const contentRef = React.useRef<HTMLDivElement>(null);
  const containerRef = React.useRef<HTMLDivElement>(null);
  const gutterRef = React.useRef<HTMLDivElement>(null);

  const [gutterWidth, setGutterWidth] = React.useState<number | undefined>(undefined);
  useLayoutEffect(() => {
    const width = gutterRef?.current ? getWidth(gutterRef.current) : undefined;
    if (width && gutterWidth !== width) {
      setGutterWidth(width);
    }
  });

  useLayoutEffect(() => {
    const diffMillis = diff(merge(focusedTime, min), focusedTime, 'milliseconds');
    const totalMillis = diff(min, max, 'milliseconds');
    const _scrollRatio = diffMillis / totalMillis;
    const content = contentRef.current;
    if (content) {
      content.scrollTop = content.scrollHeight * _scrollRatio;
    }
  });

  const showMultiDayTimes = true;
  let allDayEvents: Event[] = [];
  let rangeEvents: Event[] = [];
  events.forEach((event) => {
    if (inEventRange(event, dateRange)) {
      if (
        event.allDay ||
        startAndEndAreDateOnly(event.start, event.end) ||
        (!showMultiDayTimes && !eq(event.start, event.end, 'day'))
      ) {
        allDayEvents.push(event);
      } else {
        rangeEvents.push(event);
      }
    }
  });

  let rangeBackgroundEvents: Event[] = [];
  backgroundEvents.forEach((event) => {
    if (inEventRange(event, dateRange)) {
      rangeBackgroundEvents.push(event);
    }
  });

  allDayEvents.sort(sortEvents);
  const grouper = makeGrouper(resources);
  const groupedEvents = grouper.groupEvents(rangeEvents);
  const groupedBackgroundEvents = grouper.groupEvents(rangeBackgroundEvents);

  return (
    <div
      ref={containerRef}
      className={clsx('rbc-time-view', resources && 'rbc-time-view-resources')}
    >
      <TimeGridHeader
        range={range}
        events={allDayEvents}
        width={gutterWidth}
        resources={grouper}
        scrollRef={scrollRef}
      />
      <div
        ref={contentRef}
        className="rbc-time-content"
        onScroll={(e) => {
          if (scrollRef.current) {
            scrollRef.current.scrollLeft = e.currentTarget.scrollLeft;
          }
        }}
      >
        <TimeGutter className="rbc-time-gutter" gutterRef={gutterRef} date={dateRange.start} />

        {grouper.map(([resource, id], i) =>
          range.map((date, jj) => (
            <DayColumn
              resourceId={resource && id}
              key={i + '-' + jj}
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

export default TimeGrid;
