import React from 'react'
import clsx from 'clsx'
import { getSlotMetrics } from './TimeSlotMetrics'
import { add, format, merge } from './localizer'
import { NavigationContext } from './NavigationContext'

type TimeGutterProps = {
  className: string;
  date: Date;
  gutterRef: React.ForwardedRef<HTMLDivElement>
}

const TimeGutter = ({ gutterRef, className, date }: TimeGutterProps) => {
  let { min, max, timeslots, step } = React.useContext(NavigationContext);

  const slotMetrics = React.useMemo(() => {
    min = merge(date, min);
    max = merge(date, max);
    if (min.getTimezoneOffset() !== max.getTimezoneOffset()) {
      min = add(min, -1, 'day');
      max = add(max, -1, 'day');
    }
    return getSlotMetrics({ min, max, timeslots, step })
  }, [date, min, max, timeslots, step])

  return (
    <div className={clsx('rbc-time-gutter rbc-time-column', className)} ref={gutterRef}>
      {slotMetrics.groups.map((group, idx) => (
        <div key={idx} className="rbc-timeslot-group">
          {group.map((value, idx) => (
            <div key={idx} className='rbc-time-slot'>
              {idx === 0 ? (
                <span key={idx} className={clsx('rbc-label', slotMetrics.dateIsInGroup(date, idx) && 'rbc-now')}>
                  {format(value, 'p')}
                </span>
              ) : null}
            </div>
          ))}
        </div>
      ))}
    </div>
  )
}

export default TimeGutter
