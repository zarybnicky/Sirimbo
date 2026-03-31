import React from 'react';
import { getSlotMetrics } from './TimeSlotMetrics';
import { shortTimeIntl } from './localizer';
import { useAtomValue } from 'jotai';
import { maxTimeAtom, minTimeAtom, stepAtom, timeslotsAtom } from './state';
import { cn } from '@/lib/cn';

type TimeGutterProps = {
  date: Date;
  gutterRef: React.ForwardedRef<HTMLDivElement>;
};

function TimeGutter({ gutterRef, date }: TimeGutterProps) {
  const minTime = useAtomValue(minTimeAtom);
  const maxTime = useAtomValue(maxTimeAtom);
  const timeslots = useAtomValue(timeslotsAtom);
  const step = useAtomValue(stepAtom);

  const slotMetrics = React.useMemo(() => {
    return getSlotMetrics({ date, minTime, maxTime, timeslots, step });
  }, [date, minTime, maxTime, timeslots, step]);

  return (
    <div className="rbc-time-gutter rbc-time-column" ref={gutterRef}>
      {slotMetrics.groups.map((group, idx) => (
        <div key={idx} className="rbc-timeslot-group">
          {group.map((value, idx) => (
            <div key={idx} className="rbc-time-slot">
              {idx === 0 && (
                <span
                  key={idx}
                  className={cn('px-1', {
                    'font-bold': slotMetrics.dateIsInGroup(date, idx),
                  })}
                >
                  {shortTimeIntl.format(value)}
                </span>
              )}
            </div>
          ))}
        </div>
      ))}
    </div>
  );
}

export default TimeGutter;
