import clsx from 'clsx';
import React, { useContext } from 'react';
import DateContentRow from './DateContentRow';
import { format, eq } from './localizer';
import { Event, SlotInfo, View } from './types';
import { ResourceGrouper } from './ResourceGrouper';
import { NavigationContext } from 'NavigationContext';

type TimeGridHeaderProps = {
  range: Date[];
  events: Event[];
  resources: ResourceGrouper;
  width?: number;
  scrollRef: any;
};

const TimeGridHeader = ({
  width,
  resources,
  range,
  events,
  scrollRef,
  ...props
}: TimeGridHeaderProps) => {
  const today = new Date();
  const groupedEvents = resources.groupEvents(events);
  const { onDrillDown } = useContext(NavigationContext);

  return (
    <div ref={scrollRef} className="rbc-time-header">
      <div
        className="rbc-label rbc-time-header-gutter"
        style={{ width, minWidth: width, maxWidth: width }}
      />

      {resources.map(([resource, id], idx) => (
        <div className="rbc-time-header-content" key={id || idx}>
          {resource && (
            <div className="rbc-row rbc-row-resource" key={`resource_${idx}`}>
              <div className="rbc-header">{resource.resourceTitle}</div>
            </div>
          )}
          <div
            className={clsx(
              'rbc-row rbc-time-header-cell',
              range.length <= 1 && 'rbc-time-header-cell-single-day',
            )}
          >
            {range.map((date, i) => (
              <div
                key={i}
                className={clsx('rbc-header', eq(date, today, 'day') && 'rbc-today')}
              >
                <button
                  type="button"
                  className="rbc-button-link"
                  onClick={(e) => {
                    e.preventDefault();
                    onDrillDown(date, View.DAY);
                  }}
                >
                  <span role="columnheader" aria-sort="none">
                    {format(date, 'dd eee')}
                  </span>
                </button>
              </div>
            ))}
          </div>

          <DateContentRow
            isAllDay
            range={range}
            events={groupedEvents.get(id) || []}
            resourceId={resource && id}
            className="rbc-allday-cell"
            {...props}
          />
        </div>
      ))}
    </div>
  );
};

export default TimeGridHeader;
