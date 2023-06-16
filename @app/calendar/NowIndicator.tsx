import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import React from 'react';
import { TimeSlotMetrics } from 'TimeSlotMetrics';

type Props = {
  date: Date;
  min: Date;
  max: Date;
  slotMetrics: TimeSlotMetrics;
}

export const NowIndicator = ({date, min, max, slotMetrics}: Props) => {
  const [top, setTop] = React.useState('');

  useLayoutEffect(() => {
    const update = () => {
      const now = new Date()
      if (now >= min && now <= max) {
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
    <div className="rbc-current-time-indicator" style={{ top }} />
  );
};
