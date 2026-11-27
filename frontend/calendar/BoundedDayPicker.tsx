import type { CalendarViewKey } from '@/calendar/CalendarViews';
import type { DateRange } from '@/calendar/types';
import {
  RadioButtonGroup,
  type RadioButtonGroupItem,
} from '@/ui/fields/RadioButtonGroupElement';
import { capitalize, dayFormatter } from '@/ui/format';
import { eachDayOfInterval, startOfDay } from 'date-fns';

const dayId = (date: Date) => String(startOfDay(date).getTime());

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
  const days = eachDayOfInterval({ start: range.since, end: range.until });

  if (days.length <= 1) return null;

  const options: RadioButtonGroupItem[] = [
    { id: 'range', label: 'Přehled' },
    ...days.map((day) => ({
      id: dayId(day),
      label: capitalize(dayFormatter.format(day)),
    })),
  ];

  return (
    <div className="scrollbar min-w-0 max-w-full grow overflow-x-auto overscroll-x-contain">
      <RadioButtonGroup
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
