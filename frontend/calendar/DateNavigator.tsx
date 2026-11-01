import type { CalendarView } from '@/calendar/CalendarViews';
import { buttonCls, buttonGroupCls } from '@/ui/style';
import { MoveLeft, MoveRight } from 'lucide-react';

export function DateNavigator({
  date,
  setDate,
  view,
  onToday,
}: {
  setDate: (x: Date) => void;
  view: CalendarView;
  date: Date;
  onToday?: () => void;
}) {
  const previous = view.nav?.(date, -1);
  const next = view.nav?.(date, 1);
  const today = new Date();

  return (
    <div className={buttonGroupCls()}>
      <button
        type="button"
        className={buttonCls({ size: 'sm', variant: 'outline', className: 'py-0' })}
        disabled={!previous}
        onClick={() => previous && setDate(previous)}
      >
        <MoveLeft className="size-6! mx-1" />
      </button>
      <button
        type="button"
        className={buttonCls({ size: 'sm', variant: 'outline' })}
        onClick={() => (onToday ? onToday() : setDate(today))}
      >
        Dnes
      </button>
      <button
        type="button"
        className={buttonCls({ size: 'sm', variant: 'outline', className: 'py-0' })}
        disabled={!next}
        onClick={() => next && setDate(next)}
      >
        <MoveRight className="size-6! mx-1" />
      </button>
    </div>
  );
}
