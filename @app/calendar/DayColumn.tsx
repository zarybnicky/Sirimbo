import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import clsx from 'clsx';
import { DnDContext } from 'DnDContext';
import { NavigationContext } from 'NavigationContext';
import React from 'react';
import { pointInColumn } from './common';
import getStyledEventsOverlap from './layout-algorithms/overlap';
import { add, diff, eq, gt, lte, max, merge, min, range, timeRangeFormat } from './localizer';
import { NowIndicator } from './NowIndicator';
import { SelectionContext } from './SelectContext';
import Selection, { getBoundsForNode, getEventNodeFromPoint, isEvent } from './Selection';
import TimeGridEvent from './TimeGridEvent';
import { getSlotMetrics } from './TimeSlotMetrics';
import { Bounds, Event, Point } from './types';

const EMPTY = {}

type DayColumnProps = {
  date: Date;
  resourceId?: number;
  events: Event[];
  backgroundEvents: Event[];
  gridRef: React.RefObject<HTMLDivElement>;
};

type BackgroundSelectionState = {
  selecting?: boolean;
  initialSlot?: Date;
  top?: string;
  height?: string;
  start?: number;
  startDate?: Date;
  end?: number;
  endDate?: Date;
};
type EventSelectionState = {
  event?: Event;
  top?: number;
  height?: number;
  eventOffsetTop?: number;
}

const DayColumn = ({ date, resourceId, events, backgroundEvents, gridRef }: DayColumnProps) => {
  const columnRef = React.useRef<HTMLDivElement>(null);
  const { onSelectSlot } = React.useContext(SelectionContext);
  const { min: minTime, max: maxTime, step, timeslots } = React.useContext(NavigationContext);
  const [backgroundState, setBackgroundState] = React.useState<BackgroundSelectionState>({});
  const { draggable } = React.useContext(DnDContext);
  const [eventState, setEventState] = React.useState<EventSelectionState>(EMPTY)
  const slotMetrics = React.useMemo(() => {
    return getSlotMetrics({
      min: merge(date, minTime),
      max: merge(date, maxTime),
      step,
      timeslots,
    });
  }, [minTime, maxTime, step, timeslots]);

  useLayoutEffect(() => {
    const selector = new Selection(() => columnRef.current);

    let selectionState = (point: Point, state: BackgroundSelectionState) => {
      const bounds = getBoundsForNode(columnRef.current!);
      let currentSlot = slotMetrics.closestSlotFromPoint(point, bounds)!;
      let initialSlot = state.initialSlot || currentSlot;

      if (lte(initialSlot, currentSlot)) {
        currentSlot = slotMetrics.nextSlot(currentSlot)!;
      } else if (gt(initialSlot, currentSlot)) {
        initialSlot = slotMetrics.nextSlot(initialSlot)!;
      }
      const selectRange = slotMetrics.getRange(initialSlot, currentSlot);
      return {
        ...selectRange,
        initialSlot: state.initialSlot || currentSlot,
        selecting: true,
        top: `${selectRange.top}%`,
        height: `${selectRange.height}%`,
      };
    };

    selector.on('beforeSelect', (box: Bounds) => !isEvent(columnRef.current, box));
    selector.on('selecting', (box: Bounds) => {
      setBackgroundState((backgroundState) => {
        let newState = selectionState(box, backgroundState);
        return backgroundState.start !== newState.start ||
          backgroundState.end !== newState.end ||
          backgroundState.selecting !== newState.selecting
          ? newState
          : backgroundState;
      });
    });
    selector.on('selectStart', (box: Bounds) => {
      setBackgroundState((backgroundState) => {
        let newState = selectionState(box, backgroundState);
        return backgroundState.start !== newState.start ||
          backgroundState.end !== newState.end ||
          backgroundState.selecting !== newState.selecting
          ? newState
          : backgroundState;
      });
    });
    selector.on('click', (box: Bounds) => {
      setBackgroundState((backgroundState) => {
        if (!isEvent(columnRef.current, box)) {
          const { startDate, endDate } = selectionState(box, backgroundState);
          onSelectSlot({
            slots: range(startDate, endDate, 'hours'),
            start: startDate,
            end: endDate,
            resourceId,
            action: 'click',
            box,
          });
        }
        return { selecting: false };
      });
    });

    selector.on('select', (bounds: Bounds) => {
      setBackgroundState((backgroundState) => {
        if (!backgroundState.selecting) return backgroundState;
        const { startDate, endDate } = backgroundState;
        onSelectSlot({
          slots: range(startDate!, endDate!, 'hours'),
          start: startDate!,
          end: endDate!,
          resourceId,
          action: 'select',
          bounds,
        });
        return { selecting: false };
      });
    });

    selector.on('reset', () => {
      setBackgroundState((state) => (state.selecting ? { selecting: false } : state));
    });

    return () => selector.teardown();
  }, []);

  useLayoutEffect(() => {
  let selector = new Selection(() => gridRef.current)

  selector.on('beforeSelect', (point: Point) => {
    const bounds = getBoundsForNode(columnRef.current!)
    if (!draggable.dragAndDropAction.current.action) return false
    if (draggable.dragAndDropAction.current.action === 'resize') {
      return pointInColumn(bounds, point)
    }

    const eventNode = getEventNodeFromPoint(columnRef.current!, point as any)
    if (!eventNode) return false

    // eventOffsetTop is distance from the top of the event to the initial
    // mouseDown position. We need this later to compute the new top of the
    // event during move operations, since the final location is really a
    // delta from this point. note: if we want to DRY this with WeekWrapper,
    // probably better just to capture the mouseDown point here and do the
    // placement computation in handleMove()...
    setEventState(x => ({ ...x, eventOffsetTop: point.y - getBoundsForNode(eventNode).top}))
  })

  selector.on('selecting', (point: Point) => {
    const { event, direction, action } = draggable.dragAndDropAction.current
    const bounds = getBoundsForNode(columnRef.current!)
    if (!event || !['move', 'resize'].includes(action ?? '')) {
      return;
    }

    setTimeout(() => {
      const draggedEl = columnRef.current!.querySelector('.rbc-addons-dnd-drag-preview') as HTMLElement;
      if (draggedEl) {
        const parent = gridRef.current!;
        if (draggedEl.offsetTop < parent.scrollTop) {
          parent.scrollTop = Math.max(draggedEl.offsetTop, 0)
        } else if (draggedEl.offsetTop + draggedEl.offsetHeight > parent.scrollTop + parent.clientHeight) {
          parent.scrollTop = Math.min(draggedEl.offsetTop - parent.offsetHeight + draggedEl.offsetHeight, parent.scrollHeight)
        }
      }
    })

    const { start, end } = event;
    const duration = diff(start, end, 'milliseconds')

    setEventState((eventState) => {
      let newRange = { startDate: event.start, endDate: event.end };
      if (action === 'move') {
        if (!pointInColumn(bounds, point)) {
          return EMPTY
        }
        const newSlot = slotMetrics.closestSlotFromPoint({ y: point.y - (eventState.eventOffsetTop ?? 0), x: point.x }, bounds)
        let newEnd = add(newSlot, duration, 'milliseconds')
        newRange = slotMetrics.getRange(newSlot, newEnd, false, true)
      } else {
        const newTime = slotMetrics.closestSlotFromPoint(point, bounds)
        if (direction === 'UP') {
          const newStart = min(newTime, slotMetrics.closestSlotFromDate(end, -1))
          newRange = {...slotMetrics.getRange(newStart, end), endDate: end}
        } else if (direction === 'DOWN') {
          const newEnd = max(newTime, slotMetrics.closestSlotFromDate(start))
          newRange = {...slotMetrics.getRange(start, newEnd), startDate: start}
        }
      }
      if (event && newRange.startDate === event.start && newRange.endDate === event.end) {
        return eventState
      }
      return { ...eventState, event: { ...event, start: newRange.startDate, end: newRange.endDate } };
    })
  })

  selector.on('dropFromOutside', (point: Point) => {
    const bounds = getBoundsForNode(columnRef.current!)
    if (pointInColumn(bounds, point)) {
      let start = slotMetrics.closestSlotFromPoint(point, bounds)
      draggable.onDropFromOutside?.({start, end: slotMetrics.nextSlot(start), allDay: false, resourceId})
    }
  })

  selector.on('dragOver', (point: Point) => {
    const bounds = getBoundsForNode(columnRef.current!)
    let start = slotMetrics.closestSlotFromPoint(point, bounds)
    draggable.onDropFromOutside?.({start, end: slotMetrics.nextSlot(start), allDay: false, resourceId})
  })

  selector.on('selectStart', () => {
    draggable.onStart()
  })

  selector.on('select', (point: Point) => {
    const bounds = getBoundsForNode(columnRef.current!)
    setEventState(({ event }) => {
      if (event && (draggable.dragAndDropAction.current.action === 'resize' || pointInColumn(bounds, point))) {
        draggable.onEnd({start: event.start, end: event.end, resourceId})
      }
      return EMPTY;
    })
  })

  selector.on('click', () => {
    setEventState(EMPTY)
    draggable.onEnd(null)
  })
  selector.on('reset', () => {
    setEventState(EMPTY)
    draggable.onEnd(null)
  })

  return () => selector.teardown()
}, [])
  const minimumStartDifference = Math.ceil((step * timeslots) / 2);

  return (
    <div
      ref={columnRef}
      className={clsx(
        'rbc-day-slot rbc-time-column',
        eq(date, new Date(), 'day') && 'rbc-now rbc-today',
        backgroundState.selecting && 'rbc-slot-selecting',
      )}
    >
      {slotMetrics.groups.map((group, idx) => (
        <div key={idx} className="rbc-timeslot-group">
          {group.map((_, idx) => (
            <div key={idx} className="rbc-time-slot" />
          ))}
        </div>
      ))}

      <div className="rbc-events-container">
        {getStyledEventsOverlap(
          backgroundEvents,
          slotMetrics,
          minimumStartDifference,
        ).map(({ event, style }) => (
          <TimeGridEvent
            isBackgroundEvent
            key={event.id}
            style={style}
            event={event}
            resourceId={resourceId}
            slotMetrics={slotMetrics}
          />
        ))}
        {getStyledEventsOverlap(events, slotMetrics, minimumStartDifference).map(
          ({ event, style }) => (
            <TimeGridEvent
              key={event.id}
              style={style}
              event={event}
              resourceId={resourceId}
              slotMetrics={slotMetrics}
            />
          ),
        )}
        {eventState.event && (
          <TimeGridEvent
            event={eventState.event}
            className="rbc-addons-dnd-drag-preview"
            style={{ top: eventState.top ?? 0, height: eventState.height ?? 0, width: 100, xOffset: 0 }}
            resourceId={resourceId}
            slotMetrics={slotMetrics}
          />
        )}
      </div>

      {backgroundState.selecting && (
        <div
          className="rbc-slot-selection"
          style={{ top: backgroundState.top, height: backgroundState.height }}
        >
          <span>{timeRangeFormat({ start: backgroundState.startDate!, end: backgroundState.endDate! })}</span>
        </div>
      )}
      <NowIndicator date={date} slotMetrics={slotMetrics} />
    </div>
  );
};

export default DayColumn;
