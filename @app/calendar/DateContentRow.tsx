import React from 'react';
import clsx from 'clsx';
import getHeight from 'dom-helpers/height';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import BackgroundCells from './BackgroundCells';
import EventRow from './EventRow';
import EventEndingRow from './EventEndingRow';
import { getSlotMetrics } from './DateSlotMetrics';
import { Event, Point } from './types';
import { Segment, eventSegments, getSlotAtX, pointInBox } from './common'
import Selection, { getBoundsForNode } from './Selection'
import { DnDContext } from './DnDContext'
import { diff, merge, add, eq, inRange, lt, gt } from './localizer'
import { startOf } from 'date-arithmetic'

type DateContentRowProps = {
  date?: Date;
  range: Date[];
  events: Event[];
  className?: string;
  renderHeader?: (x: { date: Date } & React.HTMLProps<HTMLDivElement>) => JSX.Element;
  resourceId?: number;
  isAllDay?: boolean;
  measureRows?: boolean;
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
}: DateContentRowProps) => {
  const containerRef = React.useRef<HTMLDivElement>(null);
  const headingRowRef = React.useRef<HTMLDivElement>(null);
  const eventRowRef = React.useRef<HTMLDivElement>(null);
  const draggableRef = React.useRef<HTMLDivElement>(null);
  const { draggable } = React.useContext(DnDContext);
  const [segment, setSegment] = React.useState<Segment | null>(null);
  const [maxRows, setMaxRows] = React.useState(5);
  const [previousDate, setPreviousDate] = React.useState(range[0]!);
  const [renderForMeasure, setRenderForMeasure] = React.useState(!!measureRows);

  const slotMetrics = React.useMemo(() => {
    return getSlotMetrics({ range, events, minRows: 1, maxRows });
  }, [range, events, maxRows]);

  useLayoutEffect(() => {
    const selector = new Selection(
      () => containerRef.current,
      !isAllDay ? [] : ['.rbc-day-slot', '.rbc-allday-cell'],
    )

    selector.on('beforeSelect', (point: Point) => {
      const { action } = draggable.dragAndDropAction.current
      const bounds = getBoundsForNode(containerRef.current!)
      return action === 'move' || (action === 'resize' && (!isAllDay || pointInBox(bounds, point)));
    })

    selector.on('dragOverFromOutside', (point: Point) => {
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

    selector.on('selecting', (point: Point) => {
      const { action, event, direction } = draggable.dragAndDropAction.current
      const bounds = getBoundsForNode(containerRef.current!)
      const date = slotMetrics.getDateForSlot(getSlotAtX(bounds, point.x, slotMetrics.slots))

      setSegment((segment) => {
        let { start, end } = event!
        if (action === 'move') {
          if (!pointInBox(bounds, point)) {
            return null;
          }
          start = merge(date, start)
          end = add(start, diff(start, end, 'milliseconds'), 'milliseconds')
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
            start = merge(start, event!.start)
            if (gt(start, end)) {
              start = event!.start
            }
          }
        }
        const newSegment = eventSegments({ ...event!, end, start, __isPreview: true }, slotMetrics.range)
        if (segment && segment.span === newSegment.span && segment.left === newSegment.left && segment.right === newSegment.right) {
          return segment;
        }
        return newSegment;
      })
    })

    selector.on('dropFromOutside', (point: Point) => {
      if (!draggable.onDropFromOutside) return
      const bounds = getBoundsForNode(containerRef.current!)
      if (pointInBox(bounds, point)) {
        const start = slotMetrics.getDateForSlot(getSlotAtX(bounds, point.x, slotMetrics.slots))
        const end = add(start, 1, 'day')
        draggable.onDropFromOutside({start, end, allDay: false})
      }
    })

    selector.on('selectStart', () => draggable.onStart())

    selector.on('select', (point: Point) => {
      const bounds = getBoundsForNode(containerRef.current!)
      setSegment((segment) => {
        if (segment && pointInBox(bounds, point)) {
          draggable.onEnd({start: segment.event.start, end: segment.event.end, resourceId, isAllDay})
        }
        return null;
      })
    })

    selector.on('click', () => {
      draggable.onEnd(null)
      setSegment(null);
    })

    selector.on('reset', () => {
      draggable.onEnd(null)
      setSegment(null);
    })

    return () => selector.teardown()
  }, [])

  React.useEffect(() => {
    if (range[0]!.getMonth() !== previousDate.getMonth()) {
      setRenderForMeasure(true);
    }
    setPreviousDate(range[0]!);
  }, [range]);

  useLayoutEffect(() => {
    if (renderForMeasure) {
      const eventHeight = eventRowRef.current ? getHeight(eventRowRef.current) : 0;
      const headingHeight = headingRowRef.current ? getHeight(headingRowRef.current) : 0;
      const eventSpace =
        (containerRef.current ? getHeight(containerRef.current) : 0) - headingHeight;
      setMaxRows(Math.max(Math.floor(eventSpace / eventHeight), 1));
    }
  }, [renderForMeasure]);

  return (
    <div className={className} role="rowgroup" ref={containerRef}>
      {!renderForMeasure && (
        <BackgroundCells
          date={date}
          range={range}
          container={() => containerRef.current}
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
              <EventRow className="rbc-addons-dnd-drag-row" segments={[segment]} resourceId={resourceId} slotMetrics={slotMetrics} />
            )}
          </div>
        )}
      </div>
    </div>
  );
};

export default DateContentRow;
