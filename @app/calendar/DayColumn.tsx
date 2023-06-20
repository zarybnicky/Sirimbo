import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import clsx from 'clsx';
import React from 'react';
import { DnDContext } from './DnDContext';
import getStyledEventsOverlap from './layout-algorithms/overlap';
import { add, diff, eq, gt, lte, max, merge, min, range, format } from './localizer';
import { NavigationContext } from './NavigationContext';
import { NowIndicator } from './NowIndicator';
import { SelectionContext } from './SelectContext';
import Selection, { Bounds, ClientPoint, getBoundsForNode, getEventNodeFromPoint, isEvent, pointInColumn } from './Selection';
import TimeGridEvent from './TimeGridEvent';
import { getSlotMetrics } from './TimeSlotMetrics';
import { CalendarEvent } from './types';

const EMPTY = {}

type DayColumnProps = {
  date: Date;
  resourceId?: number;
  events: CalendarEvent[];
  backgroundEvents: CalendarEvent[];
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
  event?: CalendarEvent;
  top?: number;
  height?: number;
}

const DayColumn = ({ date, resourceId, events, backgroundEvents, gridRef }: DayColumnProps) => {
  const columnRef = React.useRef<HTMLDivElement>(null);
  const eventOffsetTopRef = React.useRef<number>(0);
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
  }, [date, minTime, maxTime, step, timeslots]);

  useLayoutEffect(() => {
    const selector = new Selection(() => columnRef.current, {
      shouldSelect(point) {
        return !isEvent(columnRef.current!, point);
      },
    });

    let selectionState = (point: ClientPoint | Bounds, state: BackgroundSelectionState) => {
      const bounds = getBoundsForNode(columnRef.current!);
      let currentSlot = slotMetrics.closestSlotFromPoint(point, bounds)!;
      let initialSlot = state.initialSlot || currentSlot;

      const selectRange = slotMetrics.getRange(
        lte(initialSlot, currentSlot) ? slotMetrics.nextSlot(currentSlot) : currentSlot,
        gt(initialSlot, currentSlot) ? slotMetrics.nextSlot(initialSlot) : initialSlot,
      );
      return {
        ...selectRange,
        initialSlot,
        selecting: true,
        top: `${selectRange.top}%`,
        height: `${selectRange.height}%`,
      };
    };

    selector.addEventListener('selecting', ({ detail: point }) => {
      setBackgroundState((backgroundState) => {
        let newState = selectionState(point, backgroundState);
        return backgroundState.start !== newState.start ||
          backgroundState.end !== newState.end ||
          backgroundState.selecting !== newState.selecting
          ? newState
          : backgroundState;
      });
    });

    selector.addEventListener('selectStart', ({ detail: point }) => {
      setBackgroundState((backgroundState) => {
        let newState = selectionState(point, backgroundState);
        return backgroundState.start !== newState.start ||
          backgroundState.end !== newState.end ||
          backgroundState.selecting !== newState.selecting
          ? newState
          : backgroundState;
      });
    });

    selector.addEventListener('click', ({ detail: point }) => {
      setBackgroundState((backgroundState) => {
        if (isEvent(columnRef.current!, point)) {
          return EMPTY;
        }
        const { startDate, endDate } = selectionState(point, backgroundState);
        onSelectSlot({
          slots: range(startDate, endDate, 'hours'),
          start: startDate,
          end: endDate,
          resourceId,
          action: 'click',
          box: point,
        });
        return EMPTY
      });
    });

    selector.addEventListener('select', ({ detail: bounds }) => {
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
        return EMPTY
      });
    });

    selector.addEventListener('reset', () => {
      setBackgroundState(EMPTY);
    });

    return () => selector.teardown();
  }, []);

  useLayoutEffect(() => {
    let selector = new Selection(() => gridRef.current, {
      shouldSelect(point) {
        const bounds = getBoundsForNode(columnRef.current!)
        if (!draggable.dragAndDropAction.current.action) return false
        if (draggable.dragAndDropAction.current.action === 'resize') {
          return pointInColumn(bounds, point)
        }
        const eventNode = getEventNodeFromPoint(columnRef.current!, point as any)
        if (!eventNode) {
          return false
        }
        // eventOffsetTop is distance from the top of the event to the initial
        // mouseDown position. We need this later to compute the new top of the
        // event during move operations, since the final location is really a
        // delta from this point. note: if we want to DRY this with WeekWrapper,
        // probably better just to capture the mouseDown point here and do the
        // placement computation in handleMove()...
        eventOffsetTopRef.current = point.y - getBoundsForNode(eventNode).top;
        return true;
      },
    })

    selector.addEventListener('selecting', ({ detail: point }) => {
      const { event, direction, action } = draggable.dragAndDropAction.current
      const bounds = getBoundsForNode(columnRef.current!)
      if (!event || !['move', 'resize'].includes(action ?? '')) {
        return;
      }

      setTimeout(() => {
        const draggedEl = columnRef.current!.querySelector('.rbc-drag-preview') as HTMLElement;
        if (draggedEl) {
          const parent = gridRef.current!;
          if (draggedEl.offsetTop < parent.scrollTop) {
            parent.scrollTop = Math.max(draggedEl.offsetTop, 0)
          } else if (draggedEl.offsetTop + draggedEl.offsetHeight > parent.scrollTop + parent.clientHeight) {
            parent.scrollTop = Math.min(draggedEl.offsetTop - parent.offsetHeight + draggedEl.offsetHeight, parent.scrollHeight)
          }
        }
      })

      setEventState((eventState) => {
        const { start, end } = event;
        const duration = diff(start, end, 'milliseconds')

        let newRange = slotMetrics.getRange(start, end);
        if (action === 'move') {
          if (!pointInColumn(bounds, point)) {
            return EMPTY
          }
          const newSlot = slotMetrics.closestSlotFromPoint({ x: point.x, y: point.y - eventOffsetTopRef.current }, bounds)
          let newEnd = add(newSlot, duration, 'milliseconds')
          newRange = slotMetrics.getRange(newSlot, newEnd, false, true)
        } else {
          const newTime = slotMetrics.closestSlotFromPoint(point, bounds)
          if (direction === 'UP') {
            newRange = slotMetrics.getRange(min(newTime, slotMetrics.closestSlotFromDate(end, -1)), end)
          } else if (direction === 'DOWN') {
            newRange = slotMetrics.getRange(start, max(newTime, slotMetrics.closestSlotFromDate(start)))
          }
        }
        if (event && newRange.startDate === event.start && newRange.endDate === event.end) {
          return eventState
        }
        return { ...newRange, event: { ...event, __isPreview: true, start: newRange.startDate, end: newRange.endDate } };
      })
    })

    selector.addEventListener('dropFromOutside', ({ detail: point }) => {
      const bounds = getBoundsForNode(columnRef.current!)
      if (pointInColumn(bounds, point)) {
        let start = slotMetrics.closestSlotFromPoint(point, bounds)
        draggable.onDropFromOutside?.({start, end: slotMetrics.nextSlot(start), allDay: false, resourceId})
      }
    })

    selector.addEventListener('dragOverFromOutside', ({ detail: point }) => {
      const bounds = getBoundsForNode(columnRef.current!)
      let start = slotMetrics.closestSlotFromPoint(point, bounds)
      draggable.onDropFromOutside?.({start, end: slotMetrics.nextSlot(start), allDay: false, resourceId})
    })

    selector.addEventListener('selectStart', ({ detail: point }) => {
      const bounds = getBoundsForNode(columnRef.current!)
      if (!pointInColumn(bounds, point)) {
        return
      }
      draggable.onStart()
      const { event, action } = draggable.dragAndDropAction.current
      if (event) {
        setEventState({...slotMetrics.getRange(event.start, event.end, false, true), event})
      }
    })

    selector.addEventListener('select', ({ detail: point }) => {
      const bounds = getBoundsForNode(columnRef.current!)
      setEventState(({ event }) => {
        if (event && (draggable.dragAndDropAction.current.action === 'resize' || pointInColumn(bounds, point))) {
          setTimeout(() => {
            draggable.onEnd({start: event.start, end: event.end, resourceId})
          })
        }
        return EMPTY;
      })
    })

    selector.addEventListener('click', () => {
      setEventState(EMPTY)
      draggable.onEnd(null)
    })

    selector.addEventListener('reset', () => {
      setEventState(EMPTY)
      draggable.onEnd(null)
    })

    return () => selector.teardown()
  }, [])

  const backgroundEventsInRange = React.useMemo(() => {
    const minimumStartDifference = Math.ceil((step * timeslots) / 2);
    return getStyledEventsOverlap(backgroundEvents, slotMetrics, minimumStartDifference);
  }, [step, timeslots, backgroundEvents, slotMetrics]);

  const eventsInRange = React.useMemo(() => {
    const minimumStartDifference = Math.ceil((step * timeslots) / 2);
    return getStyledEventsOverlap(events, slotMetrics, minimumStartDifference);
  }, [step, timeslots, events, slotMetrics]);

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
        {backgroundEventsInRange.map(({ event, style }) => (
          <TimeGridEvent
            isBackgroundEvent
            key={event.id}
            style={style}
            event={event}
            resourceId={resourceId}
            slotMetrics={slotMetrics}
          />
        ))}
        {eventsInRange.map(({ event, style }) => (
          <TimeGridEvent
            key={event.id}
            style={style}
            event={event}
            resourceId={resourceId}
              slotMetrics={slotMetrics}
          />
        ))}
        {eventState.event && (
          <TimeGridEvent
            event={eventState.event}
            className="rbc-drag-preview"
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
          <span>{format(backgroundState.startDate!, 'p')} – {format(backgroundState.endDate!, 'p')}</span>
        </div>
      )}
      <NowIndicator date={date} slotMetrics={slotMetrics} />
    </div>
  );
};

export default DayColumn;
