import type { CalendarViewKey } from '@/calendar/CalendarViews';
import type { DateRange } from '@/calendar/types';
import {
  RadioButtonGroup,
  type RadioButtonGroupItem,
} from '@/ui/fields/RadioButtonGroupElement';
import { capitalize } from '@/ui/format';
import { add, startOf } from 'date-arithmetic';

const dayFormatter = new Intl.DateTimeFormat('cs-CZ', {
  weekday: 'short',
  day: 'numeric',
  month: 'numeric',
});
const dayId = (date: Date) => String(startOf(date, 'day').getTime());

export function BoundedDayPicker({
  range,
  date,
  view,
  setDate,
  setView,
}: {
  range: DateRange;
  date: Date;
  view: CalendarViewKey;
  setDate: (date: Date) => void;
  setView: (view: CalendarViewKey) => void;
}) {
  const days: Date[] = [];
  for (
    let day = startOf(range.since, 'day');
    day <= startOf(range.until, 'day');
    day = add(day, 1, 'day')
  ) {
    days.push(day);
  }

  if (days.length <= 1) return null;

  const options: RadioButtonGroupItem[] = [
    { id: 'range', label: 'Celé soustředění' },
    ...days.map((day) => ({
      id: dayId(day),
      label: capitalize(dayFormatter.format(day)),
    })),
  ];

  return (
    <div
      role="radiogroup"
      aria-label="Výběr data"
      className="scrollbar min-w-0 max-w-full basis-full overflow-x-auto overscroll-x-contain"
    >
      <RadioButtonGroup
        name="camp-calendar-range"
        value={view === 'day' ? dayId(date) : 'range'}
        options={options}
        className="w-max flex-nowrap"
        onValueChange={(value) => {
          if (value === 'range') {
            setView('range');
          } else {
            setDate(new Date(Number(value)));
            setView('day');
          }
        }}
      />
    </div>
  );
}
