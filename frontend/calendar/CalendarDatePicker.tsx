import { CalendarView } from '@/calendar/CalendarViews';
import type { DateRange } from '@/calendar/types';
import { buttonCls, buttonGroupCls } from '@/ui/style';
import { MoveLeft, MoveRight } from 'lucide-react';
import React from 'react';

export function CalendarDatePicker({
  date,
  setDate,
  view,
  bounds,
}: {
  setDate: (x: Date) => void;
  view: CalendarView;
  date: Date;
  bounds?: DateRange;
}) {
  const previous = view.nav?.(date, -1);
  const next = view.nav?.(date, 1);
  const today = new Date();
  const canShow = (candidate: Date | undefined) => {
    if (!candidate) return false;
    if (!bounds) return true;
    const range = view.range(candidate);
    return range.until >= bounds.since && range.since <= bounds.until;
  };
  const todayIsVisible = canShow(today);

  return (
    <div className={buttonGroupCls()}>
      <button
        type="button"
        className={buttonCls({ variant: 'outline', className: 'py-0' })}
        disabled={!canShow(previous)}
        onClick={() => previous && setDate(previous)}
      >
        <MoveLeft className="!size-6 mx-1" />
      </button>
      <button
        type="button"
        className={buttonCls({ variant: 'outline' })}
        disabled={!todayIsVisible}
        onClick={() => setDate(today)}
      >
        Dnes
      </button>
      <button
        type="button"
        className={buttonCls({ variant: 'outline', className: 'py-0' })}
        disabled={!canShow(next)}
        onClick={() => next && setDate(next)}
      >
        <MoveRight className="!size-6 mx-1" />
      </button>
    </div>
  );
}
