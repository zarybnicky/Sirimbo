import type { CalendarView } from '@/calendar/CalendarViews';
import { buttonCls, buttonGroupCls } from '@/ui/style';
import { ChevronLeft, ChevronRight, MoveLeft, MoveRight } from 'lucide-react';

export function DateNavigator({
  date,
  setDate,
  view,
  onToday,
  variant = 'outline',
}: {
  setDate: (x: Date) => void;
  view: CalendarView;
  date: Date;
  onToday?: () => void;
  variant?: 'outline' | 'subtle';
}) {
  const previous = view.nav?.(date, -1);
  const next = view.nav?.(date, 1);
  const today = new Date();
  const subtle = variant === 'subtle';
  const PreviousIcon = subtle ? ChevronLeft : MoveLeft;
  const NextIcon = subtle ? ChevronRight : MoveRight;
  const buttonStyle = {
    size: 'sm',
    display: subtle ? 'compact' : 'button',
    variant: subtle ? 'none' : 'outline',
  } as const;
  const subtleClassName = subtle
    ? 'px-1.5 py-1 shadow-none disabled:opacity-50'
    : undefined;

  return (
    <div className={subtle ? 'inline-flex items-center gap-0.5' : buttonGroupCls()}>
      <button
        type="button"
        className={buttonCls({ ...buttonStyle, className: [subtleClassName, 'py-0'] })}
        disabled={!previous}
        onClick={() => previous && setDate(previous)}
      >
        <PreviousIcon className={subtle ? 'size-5!' : 'size-5! mx-1'} />
      </button>
      <button
        type="button"
        className={buttonCls({ ...buttonStyle, className: subtleClassName })}
        onClick={() => (onToday ? onToday() : setDate(today))}
      >
        Dnes
      </button>
      <button
        type="button"
        className={buttonCls({ ...buttonStyle, className: [subtleClassName, 'py-0'] })}
        disabled={!next}
        onClick={() => next && setDate(next)}
      >
        <NextIcon className={subtle ? 'size-5!' : 'size-5! mx-1'} />
      </button>
    </div>
  );
}
