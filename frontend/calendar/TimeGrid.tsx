import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import classnames from 'classnames';
import getWidth from 'dom-helpers/width';
import scrollbarSize from 'dom-helpers/scrollbarSize';
import React from 'react';
import DateContentRow from './DateContentRow';
import DayColumn from './DayColumn';
import {eq, inRange} from 'date-arithmetic';
import {diff, format, inEventRange, merge, sortEvents, isJustDate} from './localizer';
import { NavigationContext } from './NavigationContext';
import makeGrouper from './ResourceGrouper';
import TimeGutter from './TimeGutter';
import { CalendarEvent, Resource } from './types';

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
  const { minTime, maxTime, focusedTime, onDrillDown } = React.useContext(NavigationContext);

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

  const showMultiDayTimes = false;
  const { grouper, groupedEvents, groupedAllDayEvents, groupedBackgroundEvents } = React.useMemo(() => {
    const dateRange = { start: range[0]!, end: range[range.length - 1]! };
    const allDayEvents: CalendarEvent[] = [];
    const rangeEvents: CalendarEvent[] = [];
    events.forEach((event) => {
      if (inEventRange(event, dateRange)) {
        if (
          event.allDay ||
          (isJustDate(event.start) && isJustDate(event.end)) ||
          (!showMultiDayTimes && !eq(event.start, event.end, 'day'))
        ) {
          allDayEvents.push(event);
        } else {
          rangeEvents.push(event);
        }
      }
    });

    const rangeBackgroundEvents: CalendarEvent[] = [];
    backgroundEvents.forEach((event) => {
      if (inEventRange(event, dateRange)) {
        rangeBackgroundEvents.push(event);
      }
    });

    allDayEvents.sort(sortEvents);
    const grouper = makeGrouper(resources);
    return {
      grouper,
      groupedEvents: grouper.groupEvents(rangeEvents),
      groupedAllDayEvents: grouper.groupEvents(allDayEvents),
      groupedBackgroundEvents: grouper.groupEvents(rangeBackgroundEvents),
    };
  }, [range, events, backgroundEvents, resources, showMultiDayTimes]);

  return (
    <div
      ref={containerRef}
      className={classnames('rbc-time-view overscroll-contain', resources && 'rbc-time-view-resources')}
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

        {grouper.map(([resource, id], i) => (
          <div className="rbc-time-header-content" key={i + id}>
            {resource && (
              <div className="rbc-row rbc-row-resource" key={`resource_${id}`}>
                <div className="rbc-header">{resource.resourceTitle}</div>
              </div>
            )}
            <div className={classnames('rbc-row', range.length <= 1 && 'hidden')}>
              {range.map((date, i) => (
                <div key={i} className={classnames('rbc-header', eq(date, today, 'day') && 'rbc-today')}>
                  <button
                    type="button"
                    className="rbc-button-link"
                    onClick={(e) => {
                      e.preventDefault();
                      onDrillDown(date, 'day');
                    }}
                  >
                    <span role="columnheader" aria-sort="none">
                      {format(date, 'dd eee')}
                    </span>
                  </button>
                </div>
              ))}
            </div>

            <DateContentRow
              isAllDay
              range={range}
              events={groupedAllDayEvents.get(id) || []}
              resourceId={resource && id}
              className="rbc-allday-cell"
            />
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

export default React.memo(TimeGrid);
