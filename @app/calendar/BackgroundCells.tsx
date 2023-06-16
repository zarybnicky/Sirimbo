import React from 'react';
import clsx from 'clsx';
import { isSameDate, neq } from './localizer';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { dateCellSelection, getSlotAtX, pointInBox } from './utils/selection';
import Selection, { getBoundsForNode, isEvent, isShowMore } from './Selection';
import { Bounds, Box, SlotInfo } from './utils/constants';

type BackgroundCellsProps = {
  container: () => HTMLDivElement | null;
  range: Date[];
  date?: Date;
  onSelectSlot: (slotInfo: SlotInfo) => void;
  resourceId?: number;
};

type SelectingState = {
  selecting: boolean;
  start?: number;
  end?: number;
  initial?: { x: number; y: number };
};

const BackgroundCells = ({
  container,
  range,
  date: currentDate,
  onSelectSlot,
  resourceId,
}: BackgroundCellsProps) => {
  const [state, setState] = React.useState<SelectingState>({ selecting: false });
  const containerRef = React.useRef<HTMLDivElement>(null);

  useLayoutEffect(() => {
    let node = containerRef.current;
    let selector = new Selection(container);

    selector.on('selecting', (box: Box) => {
      setState(({ initial, selecting, start, end }) => {
        if (!selecting) {
          initial = { x: box.x, y: box.y };
        }
        start = -1;
        end = -1;
        if (node && selector.isSelected(node)) {
          const bounds = getBoundsForNode(node);
          ({ start, end } = dateCellSelection(initial, bounds, box, range.length));
        }
        return { initial, selecting: true, start, end };
      });
    });

    selector.on('beforeSelect', (box: Box) => !isEvent(containerRef.current, box));
    selector.on('click', (box: Box) => {
      if (node && !isEvent(node, box) && !isShowMore(node, box)) {
        let rowBox = getBoundsForNode(node);
        if (!pointInBox(rowBox, box)) return;
        let currentCell = getSlotAtX(rowBox, box.x, range.length);
        if (currentCell === -1) return;
        onSelectSlot({
          // TODO: unclear
          slots: [currentCell],
          start: currentCell,
          end: currentCell,
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
            slots: [start, end],
            start,
            end,
            action: 'select',
            bounds,
            resourceId,
          }); //TODO: ???
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
            'rbc-selected-cell':
              state.selecting &&
              index >= (state.start ?? -1) &&
              index <= (state.end ?? Infinity),
            'rbc-today': isSameDate(date, new Date()),
            'rbc-off-range-bg': currentDate && neq(currentDate, date, 'month'),
          })}
        />
      ))}
    </div>
  );
};

export default BackgroundCells;
