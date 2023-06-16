import React from 'react';
import clsx from 'clsx';
import Selection, { getBoundsForNode, isEvent } from './Selection';
import { getSlotMetrics } from './TimeSlotMetrics';
import TimeGridEvent from './TimeGridEvent';
import { Bounds, Event, Point } from './types';
import { eq, gt, lte, max, min, range, timeRangeFormat } from './localizer';
import EventContainer from './EventContainerWrapper';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { SelectionContext } from './SelectContext';
import getStyledEventsOverlap from './layout-algorithms/overlap';
import { NowIndicator } from './NowIndicator';
import getStyledEventsNoOverlap from './layout-algorithms/no-overlap';

type DayColumnProps = {
  date: Date;
  resourceId?: number;
  events: Event[];
  backgroundEvents: Event[];
  min: Date;
  max: Date;
  step: number;
  timeslots: number;
};

type SelectionState = {
  selecting: boolean;
  initialSlot?: Date;
  top?: string;
  height?: string;
  start?: number;
  startDate?: Date;
  end?: number;
  endDate?: Date;
};

const DayColumn = ({
  date,
  resourceId,
  events,
  backgroundEvents,
  min: minDate,
  max: maxDate,
  step,
  timeslots,
}: DayColumnProps) => {
  const containerRef = React.useRef<HTMLDivElement>(null);
  const { onSelectSlot } = React.useContext(SelectionContext);
  const slotMetrics = React.useMemo(() => {
    return getSlotMetrics({ min: minDate, max: maxDate, step, timeslots });
  }, [min, max, step, timeslots]);
  const [state, setState] = React.useState<SelectionState>({ selecting: false });
  useLayoutEffect(() => {
    const selector = new Selection(() => containerRef.current);

    let selectionState = (point: Point, state: SelectionState) => {
      const bounds = getBoundsForNode(containerRef.current!);
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

    selector.on('beforeSelect', (box: Bounds) => !isEvent(containerRef.current, box));
    selector.on('selecting', (box: Bounds) => {
      setState((state) => {
        let newState = selectionState(box, state);
        return state.start !== newState.start ||
          state.end !== newState.end ||
          state.selecting !== newState.selecting
          ? newState
          : state;
      });
    });
    selector.on('selectStart', (box: Bounds) => {
      setState((state) => {
        let newState = selectionState(box, state);
        return state.start !== newState.start ||
          state.end !== newState.end ||
          state.selecting !== newState.selecting
          ? newState
          : state;
      });
    });
    selector.on('click', (box: Bounds) => {
      setState((state) => {
        if (!isEvent(containerRef.current, box)) {
          const { startDate, endDate } = selectionState(box, state);
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
      setState((state) => {
        if (!state.selecting) return state;
        const { startDate, endDate } = state;
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
      setState((state) => (state.selecting ? { selecting: false } : state));
    });

    return () => selector.teardown();
  }, []);

  const minimumStartDifference = Math.ceil((step * timeslots) / 2);

  return (
    <div
      ref={containerRef}
      className={clsx(
        'rbc-day-slot rbc-time-column',
        eq(date, new Date(), 'day') && 'rbc-now rbc-today',
        state.selecting && 'rbc-slot-selecting',
      )}
    >
      {slotMetrics.groups.map((group, idx) => (
        <div key={idx} className="rbc-timeslot-group">
          {group.map((_, idx) => (
            <div key={idx} className="rbc-time-slot" />
          ))}
        </div>
      ))}

      <EventContainer resourceId={resourceId} slotMetrics={slotMetrics}>
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
        </div>
      </EventContainer>

      {state.selecting && (
        <div
          className="rbc-slot-selection"
          style={{ top: state.top, height: state.height }}
        >
          <span>{timeRangeFormat({ start: state.startDate!, end: state.endDate! })}</span>
        </div>
      )}
      <NowIndicator date={date} min={minDate} max={maxDate} slotMetrics={slotMetrics}  />
    </div>
  );
};

export default DayColumn;
