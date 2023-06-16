import React from 'react';
import clsx from 'clsx';
import getHeight from 'dom-helpers/height';
import BackgroundCells from './BackgroundCells';
import EventRow from './EventRow';
import EventEndingRow from './EventEndingRow';
import { getSlotMetrics } from './utils/DateSlotMetrics';
import WeekWrapper from './WeekWrapper';
import { isSameDate } from './localizer';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { Event, SlotInfo, View } from './utils/constants';

type DateContentRowProps = {
  date?: Date;
  range: Date[];
  events: Event[];
  className?: string;
  selected?: Event;
  renderHeader?: (x: { date: Date } & React.HTMLProps<HTMLDivElement>) => JSX.Element;
  onSelectEvent: (event: Event) => void;
  onSelectSlot: (range: Date[], slotInfo: SlotInfo) => void;
  onDrillDown: (date: Date, view: View) => void;
  resourceId?: number;
  isAllDay?: boolean;
  measureRows?: boolean;
};

const DateContentRow = ({
  date,
  range,
  events,
  className,
  selected,
  renderHeader,
  onSelectEvent,
  onSelectSlot,
  onDrillDown,
  resourceId,
  isAllDay,
  measureRows,
}: DateContentRowProps) => {
  const containerRef = React.useRef<HTMLDivElement>(null);
  const headingRowRef = React.useRef<HTMLDivElement>(null);
  const eventRowRef = React.useRef<HTMLDivElement>(null);
  const [maxRows, setMaxRows] = React.useState(5);
  const [previousDate, setPreviousDate] = React.useState(range[0]);
  const [renderForMeasure, setRenderForMeasure] = React.useState(!!measureRows);

  React.useEffect(() => {
    if (range[0].getMonth() !== previousDate.getMonth()) {
      setRenderForMeasure(true);
    }
    setPreviousDate(range[0]);
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

  const metrics = getSlotMetrics({ range, events, minRows: 1, maxRows });

  return (
    <div className={className} role="rowgroup" ref={containerRef}>
      {!renderForMeasure && (
        <BackgroundCells
          date={date}
          range={range}
          container={() => containerRef.current}
          onSelectSlot={(slot) =>
            onSelectSlot(range.slice(+slot.start, +slot.end + 1), slot)
          }
          resourceId={resourceId}
        />
      )}

      <div className="rbc-row-content" role="row">
        {renderHeader && (
          <div className="rbc-row " ref={headingRowRef}>
            {range.map((date, index) =>
              renderHeader({
                date,
                key: `header_${index}`,
                className: clsx(
                  'rbc-date-cell',
                  isSameDate(date, new Date()) && 'rbc-now',
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
          <WeekWrapper isAllDay={isAllDay} slotMetrics={metrics} resourceId={resourceId}>
            {metrics.levels.map((segs, idx) => (
              <EventRow
                key={idx}
                segments={segs}
                selected={selected}
                onSelectEvent={onSelectEvent}
                resourceId={resourceId}
                slotMetrics={metrics}
              />
            ))}
            {!!metrics.extra.length && (
              <EventEndingRow
                segments={metrics.extra}
                selected={selected}
                onSelectEvent={onSelectEvent}
                resourceId={resourceId}
                slotMetrics={metrics}
                onDrillDown={onDrillDown}
              />
            )}
          </WeekWrapper>
        )}
      </div>
    </div>
  );
};

export default DateContentRow;
