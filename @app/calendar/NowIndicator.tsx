import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import React from 'react';
import { merge } from './localizer';
import { NavigationContext } from './NavigationContext';
import { TimeSlotMetrics } from './TimeSlotMetrics';

type Props = {
  date: Date;
  slotMetrics: TimeSlotMetrics;
}

export const NowIndicator = ({date, slotMetrics}: Props) => {
  const [top, setTop] = React.useState('');
  const { min, max } = React.useContext(NavigationContext);

  useLayoutEffect(() => {
    const minDate = merge(date, min);
    const maxDate = merge(date, max);

    const update = () => {
      const now = new Date()
      if (now >= minDate && now <= maxDate) {
        const top = slotMetrics.getCurrentTimePosition(now)
        setTop(`${top}%`);
      }
    }

    update();
    const interval = setInterval(update, 60000);
    return () => clearInterval(interval);
  }, [date, min, max]);

  if (!top) return null
  return (
    <div className="absolute z-[3] inset-x-0 h-px pointer-events-none rbc-current-time-indicator bg-success" style={{ top }} />
  );
};