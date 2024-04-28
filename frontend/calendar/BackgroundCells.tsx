import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import closest from 'dom-helpers/closest';
import React from 'react';
import { eq, neq } from 'date-arithmetic';
import Selection, { Bounds, getBoundsForNode, getSlotAtX, isEvent, pointInBox } from './Selection';
import { useAuth } from '@/ui/use-auth';
import { dragListenersAtom } from './state';
import { useAtomValue } from 'jotai';
import { cn } from '@/ui/cn';

type BackgroundCellsProps = {
  rowRef: React.RefObject<HTMLDivElement>;
  range: Date[];
  date?: Date;
  resourceId?: string;
};

type SelectingState = {
  selecting: boolean;
  start?: number;
  end?: number;
  initial?: Bounds;
};
const EMPTY = {selecting: false};

function BackgroundCells({
  rowRef,
  range,
  date: currentDate,
  resourceId,
}: BackgroundCellsProps) {
  const auth = useAuth();
  const [state, setState] = React.useState<SelectingState>(EMPTY);
  const cellRef = React.useRef<HTMLDivElement>(null);
  const { onSelectSlot } = useAtomValue(dragListenersAtom);

  useLayoutEffect(() => {
    if (!auth.isTrainerOrAdmin) return;

    const selector = new Selection(() => rowRef.current, {
      shouldSelect(point) {
        return !isEvent(cellRef.current!, point)
      }
    });

    selector.addEventListener('selecting', ({ detail: bounds }) => {
      const rowBox = getBoundsForNode(cellRef.current!);
      setState(({ initial, selecting }) => {
        if (!selecting) {
          initial = bounds;
        }
        if (!initial || !selector.isSelected(cellRef.current!)) {
          return { initial, selecting: true, start: -1, end: -1 };
        }
        let startIdx = -1
        let endIdx = -1

        // this row is between the current and start rows, so entirely selected
        if (bounds.top < rowBox.top && bounds.bottom > rowBox.bottom) {
          startIdx = 0
          endIdx = range.length - 1
        }

        const currentSlot = getSlotAtX(rowBox, bounds.x, range.length)
        const isCurrentRow = rowBox.top < bounds.y && rowBox.bottom > bounds.y
        const isAboveStart = initial.y > rowBox.bottom
        const isBelowStart = rowBox.top > initial.y
        if (isCurrentRow) {
          if (isBelowStart) {
            startIdx = 0
            endIdx = currentSlot
          } else if (isAboveStart) {
            startIdx = currentSlot
            endIdx = range.length - 1
          }
        }

        const cellWidth = (rowBox.right - rowBox.left) / range.length
        const isStartRow = rowBox.top < initial.y && rowBox.bottom > initial.y
        if (isStartRow) {
          startIdx = endIdx = Math.floor((initial.x - rowBox.left) / cellWidth)
          if (isCurrentRow) {
            if (currentSlot < startIdx) {
              startIdx = currentSlot
            } else {
              endIdx = currentSlot //select current range
            }
          } else if (initial.y < bounds.y) {
            // the current row is below start row
            // select cells to the right of the start cell
            endIdx = range.length - 1
          } else {
            // select cells to the left of the start cell
            startIdx = 0
          }
        }
        return { initial, selecting: true, start: startIdx, end: endIdx };
      });
    });

    selector.addEventListener('click', ({ detail: point }) => {
      setState(() => {
        const target = document.elementFromPoint(point.clientX, point.clientY)!
        if (isEvent(cellRef.current!, point)) {
          return EMPTY
        }
        if (closest(target, '.rbc-show-more', cellRef.current!)) {
          return EMPTY
        }
        const rowBox = getBoundsForNode(cellRef.current!);
        if (!pointInBox(rowBox, point)) {
          return EMPTY
        }
        const currentSlot = getSlotAtX(rowBox, point.x, range.length);
        if (currentSlot !== -1) {
          onSelectSlot({
            slots: [range[currentSlot]!],
            start: range[currentSlot]!,
            end: range[currentSlot]!,
            action: 'click',
            box: point,
            resourceId,
          });
        }
        return EMPTY;
      })
    });

    selector.addEventListener('select', ({ detail: bounds }) => {
      setState(({ start, end }) => {
        if (start && end && end !== -1 && start !== -1) {
          onSelectSlot({
            slots: range.slice(start, end + 1),
            start: range[start]!,
            end: range[end]!,
            action: 'select',
            bounds,
            resourceId,
          });
        }
        return EMPTY
      });
    });

    selector.addEventListener('reset', () => {
      setState(EMPTY);
    })

    return () => selector.teardown();
  }, [auth.isTrainerOrAdmin, onSelectSlot, range, resourceId, rowRef]);

  return (
    <div className="rbc-row-bg" ref={cellRef}>
      {range.map((date, index) => (
        <div
          key={index}
          className={cn({
            'rbc-day-bg': true,
            'rbc-selected-cell': state.selecting && index >= (state.start ?? -1) && index <= (state.end ?? Infinity),
            'rbc-today': eq(date, new Date(), 'day'),
            'rbc-off-range-bg': (() => {
              console.log(currentDate, date);
              return currentDate && neq(currentDate, date, 'month');
            })(),
          })}
        />
      ))}
    </div>
  );
};

export default BackgroundCells;
