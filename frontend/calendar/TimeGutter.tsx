import React from 'react'
import { getSlotMetrics } from './TimeSlotMetrics'
import { shortTimeIntl } from './localizer'
import { useAtomValue } from 'jotai'
import { maxTimeAtom, minTimeAtom, stepAtom, timeslotsAtom } from './state'
import { cn } from '@/ui/cn'

type TimeGutterProps = {
  className: string;
  date: Date;
  gutterRef: React.ForwardedRef<HTMLDivElement>
}

function TimeGutter({ gutterRef, className, date }: TimeGutterProps) {
  const minTime = useAtomValue(minTimeAtom);
  const maxTime = useAtomValue(maxTimeAtom);
  const timeslots = useAtomValue(timeslotsAtom);
  const step = useAtomValue(stepAtom);

  const slotMetrics = React.useMemo(() => {
    return getSlotMetrics({ date, minTime, maxTime, timeslots, step })
  }, [date, minTime, maxTime, timeslots, step])

  return (
    <div className={cn('rbc-time-gutter rbc-time-column', className)} ref={gutterRef}>
      {slotMetrics.groups.map((group, idx) => (
        <div key={idx} className="rbc-timeslot-group">
          {group.map((value, idx) => (
            <div key={idx} className='rbc-time-slot'>
              {idx === 0 && (
                <span key={idx} className={cn('px-1', slotMetrics.dateIsInGroup(date, idx) && 'rbc-now')}>
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
