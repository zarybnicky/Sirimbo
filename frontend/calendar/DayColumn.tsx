import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { add, eq, gt, lte, max, min } from 'date-arithmetic';
import React from 'react';
import { NowIndicator } from './NowIndicator';
import Selection, {
  type Bounds,
  type ClientPoint,
  eventTargetsNode,
  getBoundsForNode,
  isEvent,
  pointInColumn,
} from './Selection';
import TimeGridEvent from './TimeGridEvent';
import { getSlotMetrics } from './TimeSlotMetrics';
import { diff, format, range } from './localizer';
import type { CalendarEvent, CalendarInstanceEvent, Resource } from './types';
import { useAuth } from '@/ui/use-auth';
import {
  dragListenersAtom,
  dragSubjectAtom,
  externalDragSubjectAtom,
  isDraggingAtom,
  maxTimeAtom,
  minTimeAtom,
  readExternalDragSubject,
  stepAtom,
  timeslotsAtom,
} from './state';
import { useAtomValue, useSetAtom, useStore } from 'jotai';
import { cn } from '@/lib/cn';
import { layoutEvents } from '@/calendar/layout';

const EMPTY = {};

const calendarEventKey = (event: CalendarEvent) =>
  event.kind === 'event' ? event.instance.id : event.id;

function pointTargetsDate(
  grid: HTMLElement,
  date: Date,
  { x, y }: { x: number; y: number },
) {
  return [...grid.querySelectorAll<HTMLElement>(`[data-calendar-date="${+date}"]`)].some(
    (column) => {
      const { left, right, top, bottom } = getBoundsForNode(column);
      return x >= left && x < right && y >= top && y <= bottom;
    },
  );
}

type DayColumnProps = {
  date: Date;
  resource?: Resource;
  events: CalendarEvent[];
  backgroundEvents: CalendarEvent[];
  gridRef: React.RefObject<HTMLDivElement | null>;
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
  event?: CalendarInstanceEvent;
  top?: number;
  height?: number;
};

function DayColumn({
  date,
  resource,
  events,
  backgroundEvents,
  gridRef,
}: DayColumnProps) {
  const columnRef = React.useRef<HTMLDivElement>(null);
  const eventOffsetTopRef = React.useRef<number>(0);
  const setIsDragging = useSetAtom(isDraggingAtom);
  const setDragSubject = useSetAtom(dragSubjectAtom);
  const setExternalDragSubject = useSetAtom(externalDragSubjectAtom);
  const store = useStore();

  const auth = useAuth();
  const { onSelectSlot, onMove, onResize, onRemove, onDropFromOutside } =
    useAtomValue(dragListenersAtom);
  const minTime = useAtomValue(minTimeAtom);
  const maxTime = useAtomValue(maxTimeAtom);
  const timeslots = useAtomValue(timeslotsAtom);
  const step = useAtomValue(stepAtom);
  const externalDragSubject = useAtomValue(externalDragSubjectAtom);

  const [backgroundState, setBackgroundState] = React.useState<BackgroundSelectionState>(
    {},
  );
  const [eventState, setEventState] = React.useState<EventSelectionState>(EMPTY);

  useLayoutEffect(() => {
    if (!externalDragSubject) setBackgroundState(EMPTY);
  }, [externalDragSubject]);

  const slotMetrics = React.useMemo(() => {
    return getSlotMetrics({ date, minTime, maxTime, step, timeslots });
  }, [date, minTime, maxTime, step, timeslots]);

  useLayoutEffect(() => {
    if (!auth.isTrainerOrAdmin) return;

    const selector = new Selection(() => columnRef.current, {
      shouldSelect(point) {
        return !isEvent(columnRef.current!, point);
      },
    });

    const selectionState = (
      point: ClientPoint | Bounds,
      state: BackgroundSelectionState,
    ) => {
      const bounds = getBoundsForNode(columnRef.current!);
      const currentSlot = slotMetrics.closestSlotFromPoint(point, bounds);
      const initialSlot = state.initialSlot || currentSlot;

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
        const newState = selectionState(point, backgroundState);
        return backgroundState.start !== newState.start ||
          backgroundState.end !== newState.end ||
          backgroundState.selecting !== newState.selecting
          ? newState
          : backgroundState;
      });
    });

    selector.addEventListener('selectStart', ({ detail: point }) => {
      setBackgroundState((backgroundState) => {
        const newState = selectionState(point, backgroundState);
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
        onSelectSlot?.({
          slots: range(startDate, endDate, 'hours'),
          start: startDate,
          end: endDate,
          resource,
          action: 'click',
          box: point,
        });
        return EMPTY;
      });
    });

    selector.addEventListener('select', ({ detail: bounds }) => {
      setBackgroundState((backgroundState) => {
        if (!backgroundState.selecting) return backgroundState;
        const { startDate, endDate } = backgroundState;
        onSelectSlot?.({
          slots: range(startDate!, endDate!, 'hours'),
          start: startDate!,
          end: endDate!,
          resource,
          action: 'select',
          bounds,
        });
        return EMPTY;
      });
    });

    selector.addEventListener('reset', () => {
      setBackgroundState(EMPTY);
    });

    return () => selector.teardown();
  }, [onSelectSlot, resource, slotMetrics, auth.isTrainerOrAdmin]);

  useLayoutEffect(() => {
    if (!auth.isTrainerOrAdmin) return;

    const selector = new Selection(() => gridRef.current, {
      shouldSelect(point) {
        const bounds = getBoundsForNode(columnRef.current!);
        const dragSubject = store.get(dragSubjectAtom);
        if (!dragSubject?.action) return false;
        if (dragSubject?.action === 'resize') {
          return pointInColumn(bounds, point);
        }
        const target = document.elementFromPoint(point.clientX, point.clientY)!;
        const eventNode = target.closest('.rbc-event');
        if (!eventNode || !gridRef.current?.contains(eventNode)) {
          return false;
        }
        // Move gestures are grid-scoped so all columns can participate once the
        // pointer crosses into them. Keep the initial pointer offset relative to
        // the dragged event so each column can project the preview consistently.
        eventOffsetTopRef.current =
          point.y - getBoundsForNode(eventNode as HTMLElement).top;
        return true;
      },
    });

    selector.addEventListener('selecting', ({ detail: point }) => {
      const { event, direction, action } = store.get(dragSubjectAtom) || {};
      const bounds = getBoundsForNode(columnRef.current!);
      if (!event || !['move', 'resize'].includes(action ?? '')) {
        return;
      }

      requestAnimationFrame(() => {
        const draggedEl = columnRef.current?.querySelector(
          '.rbc-drag-preview',
        ) as HTMLElement;
        const parent = gridRef.current;
        if (!draggedEl || !parent) return;

        const elTop = draggedEl.offsetTop;
        const elBottom = elTop + draggedEl.offsetHeight;
        const viewTop = parent.scrollTop;
        const viewBottom = viewTop + parent.clientHeight;

        if (elTop < viewTop) {
          parent.scrollTop = elTop;
        } else if (elBottom > viewBottom) {
          parent.scrollTop = elBottom - parent.clientHeight;
        }
      });

      setEventState((eventState) => {
        const { start, end } = event;
        const duration = diff(start, end, 'milliseconds');

        let newRange = slotMetrics.getRange(start, end);
        if (action === 'move') {
          if (!pointInColumn(bounds, point)) {
            return EMPTY;
          }
          const newSlot = slotMetrics.closestSlotFromPoint(
            { x: point.x, y: point.y - eventOffsetTopRef.current },
            bounds,
          );
          const newEnd = add(newSlot, duration, 'milliseconds');
          newRange = slotMetrics.getRange(newSlot, newEnd, false, true);
        } else {
          const newTime = slotMetrics.closestSlotFromPoint(point, bounds);
          if (direction === 'UP') {
            newRange = slotMetrics.getRange(
              min(newTime, slotMetrics.closestSlotFromDate(end, -1)),
              end,
            );
          } else if (direction === 'DOWN') {
            newRange = slotMetrics.getRange(
              start,
              max(newTime, slotMetrics.closestSlotFromDate(start)),
            );
          }
        }
        if (
          eventState.event &&
          +newRange.startDate === +eventState.event.start &&
          +newRange.endDate === +eventState.event.end
        ) {
          return eventState;
        }
        return {
          ...newRange,
          event: {
            ...event,
            __isPreview: true,
            start: newRange.startDate,
            end: newRange.endDate,
          },
        };
      });
    });

    selector.addEventListener('dragOverFromOutside', ({ detail: point }) => {
      const subject = readExternalDragSubject(
        point.dataTransfer,
        store.get(externalDragSubjectAtom),
      );
      const bounds = getBoundsForNode(columnRef.current!);
      if (
        !subject ||
        (subject.resourceId
          ? subject.resourceId !== resource?.resourceId ||
            !pointTargetsDate(gridRef.current!, date, point)
          : !eventTargetsNode(columnRef.current!, point.target))
      ) {
        setBackgroundState(EMPTY);
        return;
      }

      if (point.dataTransfer) point.dataTransfer.dropEffect = 'copy';
      const start = slotMetrics.closestSlotFromPoint(point, bounds);
      const end = add(start, subject.durationMinutes, 'minutes');
      const preview = slotMetrics.getRange(start, end, false, true);
      setBackgroundState({
        ...preview,
        selecting: true,
        top: `${preview.top}%`,
        height: `${preview.height}%`,
      });
      setIsDragging(true);
    });

    selector.addEventListener('dropFromOutside', ({ detail: point }) => {
      const subject = readExternalDragSubject(
        point.dataTransfer,
        store.get(externalDragSubjectAtom),
      );
      const bounds = getBoundsForNode(columnRef.current!);
      setBackgroundState(EMPTY);
      if (
        !subject ||
        (subject.resourceId
          ? subject.resourceId !== resource?.resourceId ||
            !pointTargetsDate(gridRef.current!, date, point)
          : !eventTargetsNode(columnRef.current!, point.target))
      )
        return;

      const start = slotMetrics.closestSlotFromPoint(point, bounds);
      void onDropFromOutside?.(subject, {
        start,
        end: add(start, subject.durationMinutes, 'minutes'),
        resource,
      });
      setExternalDragSubject(null);
      setIsDragging(false);
    });

    selector.addEventListener('selectStart', ({ detail: point }) => {
      const bounds = getBoundsForNode(columnRef.current!);
      if (!pointInColumn(bounds, point)) {
        return;
      }
      const { event } = store.get(dragSubjectAtom) || {};
      if (event) {
        setIsDragging(true);
        setEventState({
          ...slotMetrics.getRange(event.start, event.end, false, true),
          event: { ...event, __isPreview: true },
        });
      }
    });

    const reset = () => {
      setEventState(EMPTY);
      setIsDragging(false);
      setDragSubject(null);
    };

    selector.addEventListener('select', ({ detail: point }) => {
      const bounds = getBoundsForNode(columnRef.current!);
      const target = document.elementFromPoint(
        point.x - window.scrollX,
        point.y - window.scrollY,
      );
      const removeTarget = target?.closest('[data-calendar-remove-target]');
      const dragSubject = store.get(dragSubjectAtom);
      if (!target?.closest('[data-calendar-date]')) {
        reset();
        if (removeTarget && dragSubject?.action === 'move' && dragSubject.event) {
          void onRemove?.(dragSubject.event);
        }
        return;
      }
      setEventState(({ event }) => {
        const { action } = store.get(dragSubjectAtom) || {};
        if (
          event &&
          (action === 'resize' ||
            (action === 'move' && pointInColumn(bounds, point)))
        ) {
          setIsDragging(false);
          setDragSubject(null);
          if (action === 'move') {
            onMove?.(event, { start: event.start, end: event.end, resource });
          } else if (action === 'resize') {
            onResize?.(event, { start: event.start, end: event.end, resource });
          }
        }
        return EMPTY;
      });
    });

    selector.addEventListener('click', reset);
    selector.addEventListener('reset', reset);

    return () => selector.teardown();
  }, [
    setIsDragging,
    gridRef,
    resource,
    slotMetrics,
    onMove,
    onResize,
    onRemove,
    onDropFromOutside,
    auth.isTrainerOrAdmin,
    store,
    setDragSubject,
    setExternalDragSubject,
    date,
  ]);

  const backgroundEventsInRange = React.useMemo(() => {
    return layoutEvents(backgroundEvents, slotMetrics, 5);
  }, [backgroundEvents, slotMetrics]);
  const eventsInRange = React.useMemo(() => {
    return layoutEvents(events, slotMetrics, 5); // allow 5 minute overlaps
  }, [events, slotMetrics]);

  const isToday = eq(date, new Date(), 'day');

  return (
    <div
      ref={columnRef}
      data-calendar-date={+date}
      className={cn('rbc-day-slot rbc-time-column', {
        'bg-accent-3/80': isToday,
        'rbc-slot-selecting': backgroundState.selecting,
      })}
    >
      {slotMetrics.groups.map((group, idx) => (
        <div
          key={idx}
          className={cn('rbc-timeslot-group', {
            'border-accent-5': isToday,
          })}
        >
          {group.map((_, idx) => (
            <div
              key={idx}
              className={cn('rbc-time-slot border-t border-neutral-3/80', {
                'border-accent-4': isToday,
              })}
            />
          ))}
        </div>
      ))}

      <div className="absolute inset-0 mr-[4px] md:mr-[8px] lg:mr-[16px]">
        {backgroundEventsInRange.map(({ event, style }) => (
          <TimeGridEvent
            isBackgroundEvent
            key={calendarEventKey(event)}
            style={style}
            event={event}
            slotMetrics={slotMetrics}
          />
        ))}
        {eventsInRange.map(({ event, style }) => (
          <TimeGridEvent
            key={calendarEventKey(event)}
            style={style}
            event={event}
            slotMetrics={slotMetrics}
          />
        ))}
        {eventState.event && (
          <TimeGridEvent
            event={eventState.event}
            className="rbc-drag-preview"
            style={{
              top: eventState.top ?? 0,
              height: eventState.height ?? 0,
              width: 100,
              xOffset: 0,
            }}
            slotMetrics={slotMetrics}
          />
        )}
      </div>

      {backgroundState.startDate && backgroundState.endDate && (
        <div
          className="absolute z-10 w-full p-[3px] text-xs bg-neutral-9 text-white"
          style={{ top: backgroundState.top, height: backgroundState.height }}
        >
          <span>
            {format(backgroundState.startDate, 'p')} –{' '}
            {format(backgroundState.endDate, 'p')}
          </span>
        </div>
      )}
      <NowIndicator date={date} slotMetrics={slotMetrics} />
    </div>
  );
}

export default DayColumn;
