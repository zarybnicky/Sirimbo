import * as React from 'react';
import { Control, FieldValues, ControllerProps, Path, useController } from 'react-hook-form';
import { DatePickerCalendar, DateRangePicker, DatePicker } from "@axel-dev/react-nice-dates";
import '@axel-dev/react-nice-dates/build/style.css';
import cs from 'date-fns/locale/cs';

type DateRange = [Date | undefined, Date | undefined];

type DateRangeInputProps<T extends FieldValues> = {
  validation?: ControllerProps['rules'];
  name: Path<T>;
  control?: Control<T>;
  label?: React.ReactNode;
  required?: boolean;
}

export function DateRangeInput<TFieldValues extends FieldValues>({
  name, control, validation = {}, label, required,
}: DateRangeInputProps<TFieldValues>) {
  if (required && !validation?.required) {
    validation.required = 'Toto pole je povinné';
  }

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
          {label}
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

export function DatePickerElement<TFieldValues extends FieldValues>({
  name, control, validation = {}, label, required,
}: DateRangeInputProps<TFieldValues>) {
  if (required && !validation?.required) {
    validation.required = 'Toto pole je povinné';
  }
  const { field: { value, onChange } } = useController({ control, name, rules: validation });
  return <DatePicker date={value} onDateChange={onChange} locale={cs}>
    {({ inputProps }) => <>
      {label}
      <input className="input" {...inputProps} placeholder="Date" />
    </>}
  </DatePicker>;
}

export function DateCalendarElement<TFieldValues extends FieldValues>({
  name, control, validation
}: DateRangeInputProps<TFieldValues>) {
  const { field: { value, onChange } } = useController({ control, name, rules: validation });
  return <DatePickerCalendar date={value} onDateChange={onChange} locale={cs} />;
}
