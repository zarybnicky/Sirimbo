import * as React from 'react';
import format from 'date-fns/format';
import { cs } from 'date-fns/locale'
import { Control, FieldValues, ControllerProps, Path, useController } from 'react-hook-form';
import { DatePickerCalendar, DateRangePicker } from "@axel-dev/react-nice-dates";

interface DateRangeProps {
  noYear?: boolean;
  from: string;
  to?: string;
}

export function DateRange({ noYear, from, to }: DateRangeProps) {
  const f = noYear ? 'd. M.' : 'd. M. y';
  if (to && from != to) {
    return <>{format(new Date(from), f) + ' - ' + format(new Date(to), f)}</>;
  }
  return <>{format(new Date(from), f)}</>;
}

type DateRange = [Date | undefined, Date | undefined];

type DateRangeInputProps<T extends FieldValues> = {
  validation?: ControllerProps['rules'];
  name: Path<T>;
  control?: Control<T>;
}

export function DateRangeInput<TFieldValues extends FieldValues>({
  name, control, validation
}: DateRangeInputProps<TFieldValues>) {
  const { field: { value, onChange } } = useController({ control, name, rules: validation });

  return (
    <DateRangePicker
      startDate={value[0]}
      endDate={value[1]}
      onStartDateChange={(newStartDate) => onChange([newStartDate, value[1]])}
      onEndDateChange={(newEndDate) => onChange([value[0], newEndDate])}
      locale={cs}
    >
      {({ startDateInputProps, endDateInputProps }) => (
        <div className="date-range flex flex-col gap-4">
          <input
            className="input"
            {...startDateInputProps}
            placeholder="From"
          />
          <span className="date-range_arrow" />
          <input className="input" {...endDateInputProps} placeholder="To" />
        </div>
      )}
    </DateRangePicker>
  );
};


export function DateInput<TFieldValues extends FieldValues>({
  name, control, validation
}: DateRangeInputProps<TFieldValues>) {
  const { field: { value, onChange } } = useController({ control, name, rules: validation });
  return (
    <DatePickerCalendar date={value} onDateChange={onChange} locale={cs} />
  );
}
