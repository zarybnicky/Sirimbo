import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import clsx from 'clsx';
import closest from 'dom-helpers/closest';
import React from 'react';
import { eq, neq } from './localizer';
import { SelectionContext } from './SelectContext';
import Selection, { Bounds, getBoundsForNode, getSlotAtX, isEvent, pointInBox } from './Selection';

type BackgroundCellsProps = {
  rowRef: React.RefObject<HTMLDivElement>;
  range: Date[];
  date?: Date;
  resourceId?: number;
};

type SelectingState = {
  selecting: boolean;
  start?: number;
  end?: number;
  initial?: Bounds;
};
const EMPTY = {selecting: false};

const BackgroundCells = ({
  rowRef,
  range,
  date: currentDate,
  resourceId,
}: BackgroundCellsProps) => {
  const [state, setState] = React.useState<SelectingState>(EMPTY);
  const cellRef = React.useRef<HTMLDivElement>(null);
  const { onSelectSlot } = React.useContext(SelectionContext);

  useLayoutEffect(() => {
    let selector = new Selection(() => rowRef.current, {
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
        if (initial && selector.isSelected(cellRef.current!)) {
          let startIdx = -1
          let endIdx = -1
          let lastSlotIdx = range.length - 1

          let cellWidth = (rowBox.right - rowBox.left) / range.length
          let currentSlot = getSlotAtX(rowBox, bounds.x, range.length)

          // Identify row as either the initial row
          // or the row under the current mouse point
          let isCurrentRow = rowBox.top < bounds.y && rowBox.bottom > bounds.y
          let isStartRow = rowBox.top < initial.y && rowBox.bottom > initial.y

          // this row's position relative to the start point
          let isAboveStart = initial.y > rowBox.bottom
          let isBelowStart = rowBox.top > initial.y

          // this row is between the current and start rows, so entirely selected
          if (bounds.top < rowBox.top && bounds.bottom > rowBox.bottom) {
            startIdx = 0
            endIdx = lastSlotIdx
          }

          if (isCurrentRow) {
            if (isBelowStart) {
              startIdx = 0
              endIdx = currentSlot
            } else if (isAboveStart) {
              startIdx = currentSlot
              endIdx = lastSlotIdx
            }
          }

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
              endIdx = lastSlotIdx
            } else {
              // select cells to the left of the start cell
              startIdx = 0
            }
          }
          return { initial, selecting: true, start: startIdx, end: endIdx };
        }
        return { initial, selecting: true, start: -1, end: -1 };
      });
    });

    selector.addEventListener('click', ({ detail: point }) => {
      setState(() => {
        let target = document.elementFromPoint(point.clientX, point.clientY)!
        if (isEvent(cellRef.current!, point)) {
          return EMPTY
        }
        if (closest(target, '.rbc-show-more', cellRef.current!)) {
          return EMPTY
        }
        let rowBox = getBoundsForNode(cellRef.current!);
        if (!pointInBox(rowBox, point)) {
          return EMPTY
        }
        let currentSlot = getSlotAtX(rowBox, point.x, range.length);
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
  }, []);

  return (
    <div className="rbc-row-bg" ref={cellRef}>
      {range.map((date, index) => (
        <div
          key={index}
          className={clsx({
            'rbc-day-bg': true,
            'rbc-selected-cell': state.selecting && index >= (state.start ?? -1) && index <= (state.end ?? Infinity),
            'rbc-today': eq(date, new Date(), 'day'),
            'rbc-off-range-bg': currentDate && neq(currentDate, date, 'month'),
          })}
        />
      ))}
    </div>
  );
};

export default BackgroundCells;
