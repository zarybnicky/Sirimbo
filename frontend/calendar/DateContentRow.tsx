import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { add, eq, gt, inRange, lt, neq, startOf } from 'date-arithmetic';
import getHeight from 'dom-helpers/height';
import React from 'react';
import BackgroundCells from './BackgroundCells';
import { getSlotMetrics } from './DateSlotMetrics';
import EventEndingRow from './EventEndingRow';
import EventRow from './EventRow';
import Selection, { getBoundsForNode, getSlotAtX, pointInBox } from './Selection';
import { eventSegments, type Segment } from './common';
import { diff, format, merge } from './localizer';
import type { CalendarEvent, Resource } from './types';
import { useAuth } from '@/ui/use-auth';
import { useAtomValue, useSetAtom, useStore } from 'jotai';
import { dragListenersAtom, dragSubjectAtom, isDraggingAtom } from './state';
import { cn } from '@/ui/cn';

type DateContentRowProps = {
  date: Date;
  range: Date[];
  events: CalendarEvent[];
  resource?: Resource;
  containerRef: React.RefObject<HTMLDivElement | null>;
};

function DateContentRow({
  date: currentDate,
  range,
  events,
  resource,
  containerRef,
}: DateContentRowProps) {
  const headingRowRef = React.useRef<HTMLDivElement>(null);
  const eventRowRef = React.useRef<HTMLDivElement>(null);
  const draggableRef = React.useRef<HTMLDivElement>(null);

  const store = useStore();
  const setIsDragging = useSetAtom(isDraggingAtom);
  const setDragSubject = useSetAtom(dragSubjectAtom);
  const { onMove, onResize, onDrillDown } = useAtomValue(dragListenersAtom);

  const auth = useAuth();
  const [segment, setSegment] = React.useState<Segment | null>(null);
  const [maxRows, setMaxRows] = React.useState(5);
  const [previousDate, setPreviousDate] = React.useState(range[0]!);
  const [renderForMeasure, setRenderForMeasure] = React.useState(true);

  React.useEffect(() => {
    const update = () => setRenderForMeasure(true);
    window.addEventListener('resize', update);
    return () => window.removeEventListener('resize', update);
  }, []);

  const slotMetrics = React.useMemo(() => {
    return getSlotMetrics({ range, events, minRows: 1, maxRows });
  }, [range, events, maxRows]);

  useLayoutEffect(() => {
    if (!auth.isTrainerOrAdmin) return;

    const selector = new Selection(() => containerRef.current, {
      validContainers: [],
      shouldSelect() {
        const action = store.get(dragSubjectAtom)?.action;
        return action === 'move' || action === 'resize';
      },
    });

    /* FIXME: selector.addEventListener('dragOverFromOutside', ({ detail: point }) => {
     *   const bounds = getBoundsForNode(containerRef.current!)
     *   const event = dragFromOutsideItem?.()
     *   setSegment((segment) => {
     *     if (!event || !pointInBox(bounds, point)) {
     *       return null;
     *     }
     *     const date = slotMetrics.getDateForSlot(getSlotAtX(bounds, point.x, slotMetrics.slots))
     *     const start = merge(date, event.start)
     *     const end = add(start, diff(start, event.end, 'milliseconds'), 'milliseconds')
     *     const newSegment = eventSegments({ ...event, end, start, __isPreview: true }, slotMetrics.range)
     *     if (segment && segment.span === newSegment.span && segment.left === newSegment.left && segment.right === newSegment.right) {
     *       return segment;
     *     }
     *     return newSegment;
     *   })
     * }) */

    /* FIXME: selector.addEventListener('dropFromOutside', ({ detail: point }) => {
     *   const bounds = getBoundsForNode(containerRef.current!)
     *   if (pointInBox(bounds, point)) {
     *     const start = slotMetrics.getDateForSlot(getSlotAtX(bounds, point.x, slotMetrics.slots))
     *     const end = add(start, 1, 'day')
     *     onDropFromOutside?.({ start, end }) * /
     *   }
     *   setIsDragging(false);
     * }) */

    selector.addEventListener('selecting', ({ detail: point }) => {
      const { action, event, direction } = store.get(dragSubjectAtom) || {};
      const bounds = getBoundsForNode(containerRef.current!);
      const date = slotMetrics.getDateForSlot(
        getSlotAtX(bounds, point.x, slotMetrics.slots),
      );

      setSegment((segment) => {
        if (!event) {
          return null;
        }
        let { start, end } = event;
        if (action === 'move') {
          if (!pointInBox(bounds, point)) {
            return null;
          }
          start = merge(date, event.start);
          end = add(start, diff(event.start, event.end, 'milliseconds'), 'milliseconds');
        }
        if (action === 'resize') {
          const cursorInRow = pointInBox(bounds, point);

          if (direction === 'RIGHT') {
            if (cursorInRow) {
              if (slotMetrics.last < start) {
                return null;
              }
              end = eq(startOf(end, 'day'), end) ? add(date, 1, 'day') : date;
            } else if (
              inRange(start, slotMetrics.first, slotMetrics.last) ||
              (bounds.bottom < point.y && +slotMetrics.first > +start)
            ) {
              end = add(slotMetrics.last, 1, 'milliseconds');
            } else {
              return null;
            }
            end = merge(end, event.end);
            if (lt(end, start)) {
              end = event.end;
            }
          } else if (direction === 'LEFT') {
            if (cursorInRow) {
              if (slotMetrics.first > end) {
                return null;
              }
              start = date;
            } else if (
              inRange(end, slotMetrics.first, slotMetrics.last) ||
              (bounds.top > point.y && lt(slotMetrics.last, end))
            ) {
              start = add(slotMetrics.first, -1, 'milliseconds');
            } else {
              return null;
            }
            start = merge(start, event.start);
            if (gt(start, end)) {
              start = event.start;
            }
          }
        }
        const newSegment = eventSegments(
          { ...event, end, start, __isPreview: true },
          slotMetrics.range,
        );
        if (
          segment &&
          segment.span === newSegment.span &&
          segment.left === newSegment.left &&
          segment.right === newSegment.right
        ) {
          return segment;
        }
        return newSegment;
      });
    });

    selector.addEventListener('selecting', () => setIsDragging(true));

    selector.addEventListener('select', ({ detail: point }) => {
      const bounds = getBoundsForNode(containerRef.current!);
      setSegment((segment) => {
        if (segment && pointInBox(bounds, point)) {
          const { event, action } = store.get(dragSubjectAtom) || {};
          setIsDragging(false);
          setDragSubject(null);
          if (event) {
            if (action === 'move') {
              onMove?.(event, { start: event.start, end: event.end, resource });
            } else if (action === 'resize') {
              onResize?.(event, { start: event.start, end: event.end, resource });
            }
          }
        }
        return null;
      });
    });

    selector.addEventListener('click', () => {
      setIsDragging(false);
      setDragSubject(null);
      setSegment(null);
    });

    selector.addEventListener('reset', () => {
      setIsDragging(false);
      setDragSubject(null);
      setSegment(null);
    });

    return () => selector.teardown();
  }, [
    setIsDragging,
    setDragSubject,
    containerRef,
    resource,
    slotMetrics,
    onMove,
    onResize,
    store,
    auth.isTrainerOrAdmin,
  ]);

  React.useEffect(() => {
    if (range[0]?.getMonth() !== previousDate.getMonth()) {
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
  }, [renderForMeasure, containerRef]);

  return (
    <div
      className="flex relative flex-col flex-1 basis-0 overflow-hidden h-full divide-y divide-neutral-6"
      role="rowgroup"
      ref={containerRef}
    >
      {!renderForMeasure && (
        <BackgroundCells
          date={currentDate}
          range={range}
          rowRef={containerRef}
          resource={resource}
        />
      )}

      <div className="relative select-none z-[4]" role="row">
        <div className="rbc-row" ref={headingRowRef}>
          {range.map((date, index) => (
            <div
              key={`header_${index}`}
              className={cn('flex-1 min-w-0 pr-[5px] text-right', {
                'font-bold': eq(date, new Date(), 'day'),
                'text-neutral-9': neq(date, currentDate, 'month'),
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
                {format(date, 'dd')}
              </button>
            </div>
          ))}
        </div>

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
                resource={resource}
                slotMetrics={slotMetrics}
              />
            ))}
            {slotMetrics.extra.length > 0 && (
              <EventEndingRow
                segments={slotMetrics.extra}
                resource={resource}
                slotMetrics={slotMetrics}
              />
            )}
            {segment && (
              <EventRow
                className="absolute inset-x-0 top-0"
                segments={[segment]}
                resource={resource}
                slotMetrics={slotMetrics}
              />
            )}
          </div>
        )}
      </div>
    </div>
  );
}

export default DateContentRow;
