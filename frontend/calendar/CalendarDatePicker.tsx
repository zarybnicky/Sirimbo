import { CalendarView } from '@/calendar/CalendarViews';
import { buttonCls, buttonGroupCls } from '@/ui/style';
import { MoveLeft, MoveRight } from 'lucide-react';
import React from 'react';

export function CalendarDatePicker({
  date,
  setDate,
  view,
}: {
  setDate: (x: Date) => void;
  view: CalendarView;
  date: Date;
}) {
  return (
    <div className={buttonGroupCls()}>
      <button
        type="button"
        className={buttonCls({ variant: 'outline', className: 'py-0' })}
        onClick={() => setDate(view.nav(date, -1))}
      >
        <MoveLeft className="!size-6 mx-1" />
      </button>
      <button
        type="button"
        className={buttonCls({ variant: 'outline' })}
        onClick={() => setDate(new Date())}
      >
        Dnes
      </button>
      <button
        type="button"
        className={buttonCls({ variant: 'outline', className: 'py-0' })}
        onClick={() => setDate(view.nav(date, +1))}
      >
        <MoveRight className="!size-6 mx-1" />
      </button>
    </div>
  );
}
