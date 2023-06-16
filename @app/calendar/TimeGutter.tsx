import React from 'react'
import clsx from 'clsx'
import { getSlotMetrics } from './utils/TimeSlotMetrics'
import { add, format, getTimezoneOffset } from './localizer'

type TimeGutterProps = {
  className: string;
  date: Date;
  min: Date;
  max: Date;
  timeslots: number;
  step: number;
  gutterRef: React.ForwardedRef<HTMLDivElement>
}

const TimeGutter = ({ min, max, timeslots, step, gutterRef, className, date }: TimeGutterProps) => {
  const { start, end } = React.useMemo(() => {
    if (getTimezoneOffset(min) !== getTimezoneOffset(max)) {
      return { start: add(min, -1, 'day'), end: add(max, -1, 'day') }
    }
    return { start: min, end: max }
  }, [min?.toISOString(), max?.toISOString()])

  const slotMetrics = React.useMemo(() => {
    return getSlotMetrics({min: start, max: end, timeslots, step})
  }, [start, end, timeslots, step])

  return (
    <div className={clsx('rbc-time-gutter rbc-time-column', className)} ref={gutterRef}>
      {slotMetrics.groups.map((group, idx) => (
        <div key={idx} className="rbc-timeslot-group">
          {group.map((value, idx) => (
            <div className='rbc-time-slot'>
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
