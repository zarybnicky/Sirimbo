import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import clsx from 'clsx';
import { add, eq, gt, inRange, lt, startOf } from 'date-arithmetic';
import getHeight from 'dom-helpers/height';
import React from 'react';
import BackgroundCells from './BackgroundCells';
import { getSlotMetrics } from './DateSlotMetrics';
import { DnDContext } from './DnDContext';
import EventEndingRow from './EventEndingRow';
import EventRow from './EventRow';
import Selection, { getBoundsForNode, getSlotAtX, pointInBox } from './Selection';
import { Segment, eventSegments } from './common';
import { diff, merge } from './localizer';
import { CalendarEvent } from './types';

type DateContentRowProps = {
  date?: Date;
  range: Date[];
  events: CalendarEvent[];
  className?: string;
  renderHeader?: (x: { date: Date } & React.HTMLProps<HTMLDivElement>) => JSX.Element;
  resourceId?: number;
  isAllDay?: boolean;
  measureRows?: boolean;
  containerRef?: React.RefObject<HTMLDivElement>;
};

const DateContentRow = ({
  date,
  range,
  events,
  className,
  renderHeader,
  resourceId,
  isAllDay,
  measureRows,
  containerRef: maybeOuterContainerRef,
}: DateContentRowProps) => {
  const containerRef = React.useRef<HTMLDivElement>(null);
  const outerContainerRef = maybeOuterContainerRef || containerRef;
  const headingRowRef = React.useRef<HTMLDivElement>(null);
  const eventRowRef = React.useRef<HTMLDivElement>(null);
  const draggableRef = React.useRef<HTMLDivElement>(null);
  const draggable = React.useContext(DnDContext);
  const [segment, setSegment] = React.useState<Segment | null>(null);
  const [maxRows, setMaxRows] = React.useState(5);
  const [previousDate, setPreviousDate] = React.useState(range[0]!);
  const [renderForMeasure, setRenderForMeasure] = React.useState(!!measureRows);

  React.useEffect(() => {
    if (measureRows) {
      const update = () => setRenderForMeasure(true);
      window.addEventListener('resize', update);
      return () => window.removeEventListener('resize', update);
    }
  }, [measureRows]);

  const slotMetrics = React.useMemo(() => {
    return getSlotMetrics({ range, events, minRows: 1, maxRows });
  }, [range, events, maxRows]);

  useLayoutEffect(() => {
    const selector = new Selection(() => outerContainerRef.current, {
      validContainers: !isAllDay ? [] : ['.rbc-day-slot', '.rbc-allday-cell'],
      shouldSelect(point) {
        const { action } = draggable.stateRef.current
        const bounds = getBoundsForNode(containerRef.current!)
        return action === 'move' || (action === 'resize' && (!isAllDay || pointInBox(bounds, point)));
      },
    })

    selector.addEventListener('dragOverFromOutside', ({ detail: point }) => {
      const bounds = getBoundsForNode(containerRef.current!)
      const event = draggable.dragFromOutsideItem?.()
      setSegment((segment) => {
        if (!event || !pointInBox(bounds, point)) {
          return null;
        }
        const date = slotMetrics.getDateForSlot(getSlotAtX(bounds, point.x, slotMetrics.slots))
        const start = merge(date, event.start)
        const end = add(start, diff(start, event.end, 'milliseconds'), 'milliseconds')
        const newSegment = eventSegments({ ...event, end, start, __isPreview: true }, slotMetrics.range)
        if (segment && segment.span === newSegment.span && segment.left === newSegment.left && segment.right === newSegment.right) {
          return segment;
        }
        return newSegment;
      })
    })

    selector.addEventListener('selecting', ({ detail: point }) => {
      const { action, event, direction } = draggable.stateRef.current
      const bounds = getBoundsForNode(containerRef.current!)
      const date = slotMetrics.getDateForSlot(getSlotAtX(bounds, point.x, slotMetrics.slots))

      setSegment((segment) => {
        if (!event) {
          return null;
        }
        let { start, end } = event
        if (action === 'move') {
          if (!pointInBox(bounds, point)) {
            return null;
          }
          start = merge(date, event.start);
          end = add(start, diff(event.start, event.end, 'milliseconds'), 'milliseconds')
        }
        if (action === 'resize') {
          const cursorInRow = pointInBox(bounds, point)

          if (direction === 'RIGHT') {
            if (cursorInRow) {
              if (slotMetrics.last < start) {
                return null
              }
              end = eq(startOf(end, 'day'), end) ? add(date, 1, 'day') : date
            } else if (inRange(start, slotMetrics.first, slotMetrics.last) || (bounds.bottom < point.y && +slotMetrics.first > +start)) {
              end = add(slotMetrics.last, 1, 'milliseconds')
            } else {
              return null;
            }
            end = merge(end, event!.end)
            if (lt(end, start)) {
              end = event!.end
            }
          } else if (direction === 'LEFT') {
            if (cursorInRow) {
              if (slotMetrics.first > end) {
                return null
              }
              start = date
            } else if (inRange(end, slotMetrics.first, slotMetrics.last) || (bounds.top > point.y && lt(slotMetrics.last, end))) {
              start = add(slotMetrics.first, -1, 'milliseconds')
            } else {
              return null;
            }
            start = merge(start, event.start)
            if (gt(start, end)) {
              start = event.start
            }
          }
        }
        const newSegment = eventSegments({ ...event, end, start, __isPreview: true }, slotMetrics.range)
        if (segment && segment.span === newSegment.span && segment.left === newSegment.left && segment.right === newSegment.right) {
          return segment;
        }
        return newSegment;
      })
    })

    selector.addEventListener('dropFromOutside', ({ detail: point }) => {
      if (!draggable.onDropFromOutside) return
      const bounds = getBoundsForNode(containerRef.current!)
      if (pointInBox(bounds, point)) {
        const start = slotMetrics.getDateForSlot(getSlotAtX(bounds, point.x, slotMetrics.slots))
        const end = add(start, 1, 'day')
        draggable.onDropFromOutside({start, end, allDay: false})
      }
    })

    selector.addEventListener('selectStart', () => draggable.onStart())

    selector.addEventListener('select', ({detail:point}) => {
      const bounds = getBoundsForNode(containerRef.current!)
      setSegment((segment) => {
        if (segment && pointInBox(bounds, point)) {
          draggable.onEnd({start: segment.event.start, end: segment.event.end, resourceId, isAllDay})
        }
        return null;
      })
    })

    selector.addEventListener('click', () => {
      draggable.onEnd()
      setSegment(null);
    })

    selector.addEventListener('reset', () => {
      draggable.onEnd()
      setSegment(null);
    })

    return () => selector.teardown()
  }, [draggable, isAllDay, outerContainerRef, resourceId, slotMetrics])

  React.useEffect(() => {
    if (range[0]!.getMonth() !== previousDate.getMonth()) {
      setRenderForMeasure(true);
    }
    setPreviousDate(range[0]!);
  }, [previousDate, range]);

  useLayoutEffect(() => {
    if (renderForMeasure) {
      const eventHeight = eventRowRef.current ? getHeight(eventRowRef.current) : 0;
      const headingHeight = headingRowRef.current ? getHeight(headingRowRef.current) : 0;
      const eventSpace =
        (containerRef.current ? getHeight(containerRef.current) : 0) - headingHeight;
      setMaxRows(Math.max(Math.floor(eventSpace / eventHeight + 0.6), 1));
      setRenderForMeasure(false);
    }
  }, [renderForMeasure]);

  return (
    <div className={className} role="rowgroup" ref={containerRef}>
      {!renderForMeasure && (
        <BackgroundCells
          date={date}
          range={range}
          rowRef={outerContainerRef}
          resourceId={resourceId}
        />
      )}

      <div className="rbc-row-content" role="row">
        {renderHeader && (
          <div className="rbc-row" ref={headingRowRef}>
            {range.map((date, index) =>
              renderHeader({
                date,
                key: `header_${index}`,
                className: clsx(
                  'rbc-date-cell',
                  eq(date, new Date(), 'day') && 'rbc-now',
                ),
              }),
            )}
          </div>
        )}

        {renderForMeasure ? (
          <div className="rbc-row" ref={eventRowRef}>
            <div className="rbc-row-segment">
              <div className="rbc-event">
                <div className="rbc-event-content">&nbsp;</div>
              </div>
            </div>
          </div>
        ) : (
          <div ref={draggableRef} className="relative">
            {slotMetrics.levels.map((segs, idx) => (
              <EventRow
                key={idx}
                segments={segs}
                resourceId={resourceId}
                slotMetrics={slotMetrics}
              />
            ))}
            {!!slotMetrics.extra.length && (
              <EventEndingRow
                segments={slotMetrics.extra}
                resourceId={resourceId}
                slotMetrics={slotMetrics}
              />
            )}
            {segment && (
              <EventRow
                className="rbc-drag-row"
                segments={[segment]}
                resourceId={resourceId}
                slotMetrics={slotMetrics}
              />
            )}
          </div>
        )}
      </div>
    </div>
  );
};

export default DateContentRow;
