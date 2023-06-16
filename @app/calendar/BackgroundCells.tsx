import React from 'react';
import clsx from 'clsx';
import { eq, neq } from './localizer';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { dateCellSelection, getSlotAtX, pointInBox } from './common';
import { Bounds, Point } from './types';
import Selection, { getBoundsForNode, isEvent, isShowMore } from './Selection';
import { SelectionContext } from 'SelectContext';

type BackgroundCellsProps = {
  container: () => HTMLDivElement | null;
  range: Date[];
  date?: Date;
  resourceId?: number;
};

type SelectingState = {
  selecting: boolean;
  start?: number;
  end?: number;
  initial?: Point;
};

const BackgroundCells = ({
  container,
  range,
  date: currentDate,
  resourceId,
}: BackgroundCellsProps) => {
  const [state, setState] = React.useState<SelectingState>({ selecting: false });
  const containerRef = React.useRef<HTMLDivElement>(null);
  const { onSelectSlot } = React.useContext(SelectionContext);

  useLayoutEffect(() => {
    let node = containerRef.current;
    let selector = new Selection(container);

    selector.on('selecting', (box: Bounds) => {
      setState(({ initial, selecting, start, end }) => {
        if (!selecting) {
          initial = { x: box.x, y: box.y };
        }
        start = -1;
        end = -1;
        if (node && selector.isSelected(node)) {
          const bounds = getBoundsForNode(node);
          ({ start, end } = dateCellSelection(initial!, bounds, box, range.length));
        }
        return { initial, selecting: true, start, end };
      });
    });

    selector.on('beforeSelect', (box: Point) => !isEvent(containerRef.current, box));
    selector.on('click', (box: Point) => {
      if (node && !isEvent(node, box) && !isShowMore(node, box)) {
        let rowBox = getBoundsForNode(node);
        if (!pointInBox(rowBox, box)) return;
        let currentSlot = getSlotAtX(rowBox, box.x, range.length);
        if (currentSlot === -1) return;
        onSelectSlot({
          slots: [range[currentSlot]!],
          start: range[currentSlot]!,
          end: range[currentSlot]!,
          action: 'click',
          box,
          resourceId,
        });
      }
      setState({ selecting: false });
    });

    selector.on('select', (bounds: Bounds) => {
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
        return { selecting: false };
      });
    });
    return () => selector.teardown();
  });

  return (
    <div className="rbc-row-bg" ref={containerRef}>
      {range.map((date, index) => (
        <div
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
