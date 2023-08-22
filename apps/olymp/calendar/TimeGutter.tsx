import React from 'react'
import classNames from 'classnames'
import { getSlotMetrics } from './TimeSlotMetrics'
import { shortTimeIntl } from './localizer'
import { NavigationContext } from './NavigationContext'

type TimeGutterProps = {
  className: string;
  date: Date;
  gutterRef: React.ForwardedRef<HTMLDivElement>
}

const TimeGutter = ({ gutterRef, className, date }: TimeGutterProps) => {
  const { minTime, maxTime, timeslots, step } = React.useContext(NavigationContext);

  const slotMetrics = React.useMemo(() => {
    return getSlotMetrics({ date, minTime, maxTime, timeslots, step })
  }, [date, minTime, maxTime, timeslots, step])

  return (
    <div className={classNames('rbc-time-gutter rbc-time-column', className)} ref={gutterRef}>
      {slotMetrics.groups.map((group, idx) => (
        <div key={idx} className="rbc-timeslot-group">
          {group.map((value, idx) => (
            <div key={idx} className='rbc-time-slot'>
              {idx === 0 && (
                <span key={idx} className={classNames('px-1', slotMetrics.dateIsInGroup(date, idx) && 'rbc-now')}>
                  {shortTimeIntl.format(value)}
                </span>
              )}
            </div>
          ))}
        </div>
      ))}
    </div>
  )
}

export default TimeGutter
