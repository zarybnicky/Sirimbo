import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import React from 'react';
import { merge } from './localizer';
import type { TimeSlotMetrics } from './TimeSlotMetrics';
import { maxTimeAtom, minTimeAtom } from './state';
import { useAtomValue } from 'jotai';

type Props = {
  date: Date;
  slotMetrics: TimeSlotMetrics;
}

export const NowIndicator = React.memo(function NowIndicator({date, slotMetrics}: Props) {
  const [top, setTop] = React.useState('');
  const minTime = useAtomValue(minTimeAtom);
  const maxTime = useAtomValue(maxTimeAtom);

  useLayoutEffect(() => {
    const minDate = merge(date, minTime);
    const maxDate = merge(date, maxTime);

    const update = () => {
      const now = new Date()
      if (now >= minDate && now <= maxDate) {
        const top = slotMetrics.getCurrentTimePosition(now)
        setTop(`${top}%`);
      } else {
        setTop('');
      }
    }

    update();
    const interval = setInterval(update, 60000);
    return () => clearInterval(interval);
  }, [date, minTime, maxTime, slotMetrics]);

  if (!top) return null
  return (
    <div className="absolute z-[3] inset-x-0 h-px pointer-events-none bg-success" style={{ top }} />
  );
});
