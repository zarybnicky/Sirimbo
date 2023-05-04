import * as React from 'react';
import {
  Control,
  FieldValues,
  ControllerProps,
  Path,
  useController,
  FieldError,
} from 'react-hook-form';
import {
  DatePickerCalendar,
  DateRangePicker,
  DatePicker,
} from '@axel-dev/react-nice-dates';
import '@axel-dev/react-nice-dates/build/style.css';
import cs from 'date-fns/locale/cs';
import classNames from 'classnames';
import { ArrowRight } from 'react-feather';

export type DateRange = [Date, Date];

type DateRangeInputProps<T extends FieldValues> = {
  validation?: ControllerProps['rules'];
  name: Path<T>;
  control?: Control<T>;
  required?: boolean;
};

type Extras = {
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
  parseError?: (error: FieldError) => React.ReactNode;
};

export function DateRangeInput<TFieldValues extends FieldValues>({
  name,
  control,
  validation = {},
  label,
  required,
  className,
  helperText,
  parseError,
}: DateRangeInputProps<TFieldValues> & Extras) {
  if (required && !validation?.required) {
    validation.required = 'Toto pole je povinné';
  }

  const {
    field: { value, onChange },
    fieldState: { error },
  } = useController({ control, name, rules: validation });
  const parsedHelperText = !error
    ? helperText
    : parseError
    ? parseError(error)
    : error.message;

  return (
    <DateRangePicker
      startDate={value?.[0]}
      endDate={value?.[1]}
      onStartDateChange={(newStartDate) => onChange([newStartDate, value[1]])}
      onEndDateChange={(newEndDate) => onChange([value[0], newEndDate])}
      locale={cs}
    >
      {({ startDateInputProps, endDateInputProps }) => (
        <div className={className}>
          <label htmlFor={name} className="block text-sm font-medium text-gray-700 my-1">
            {label}
          </label>
          <div className="date-range flex flex-row gap-4">
            <input
              className="block w-full border-red-400 text-stone-900 placeholder-red-300 focus:outline-none focus:ring-red-500 focus:border-red-500 sm:text-sm rounded-md"
              {...startDateInputProps}
              placeholder="Od"
            />
            <ArrowRight className="shrink-0 self-center" />
            <input
              className="block w-full border-red-400 text-stone-900 placeholder-red-300 focus:outline-none focus:ring-red-500 focus:border-red-500 sm:text-sm rounded-md"
              {...endDateInputProps}
              placeholder="Do"
            />
          </div>
          {parsedHelperText && (
            <p
              className={classNames(
                'mt-2 text-sm',
                error ? 'text-red-600' : 'text-gray-500',
              )}
            >
              {parsedHelperText}
            </p>
          )}
        </div>
      )}
    </DateRangePicker>
  );
}

export function DatePickerElement<TFieldValues extends FieldValues>({
  name,
  control,
  validation = {},
  label,
  required,
  className,
  helperText,
  parseError,
}: DateRangeInputProps<TFieldValues> & Extras) {
  if (required && !validation?.required) {
    validation.required = 'Toto pole je povinné';
  }
  const {
    field: { value, onChange },
    fieldState: { error },
  } = useController({ control, name, rules: validation });
  const parsedHelperText = !error
    ? helperText
    : parseError
    ? parseError(error)
    : error.message;

  return (
    <DatePicker date={value} onDateChange={onChange} locale={cs}>
      {({ inputProps }) => (
        <div className={className}>
          <label htmlFor={name} className="block text-sm font-medium text-gray-700 my-1">
            {label}
          </label>
          <input
            className="block w-full border-red-400 text-stone-900 placeholder-red-300 focus:outline-none focus:ring-red-500 focus:border-red-500 sm:text-sm rounded-md"
            {...inputProps}
            placeholder="Datum"
          />
          {parsedHelperText && (
            <p
              className={classNames(
                'mt-2 text-sm',
                error ? 'text-red-600' : 'text-gray-500',
              )}
            >
              {parsedHelperText}
            </p>
          )}
        </div>
      )}
    </DatePicker>
  );
}

export function DateCalendarElement<TFieldValues extends FieldValues>({
  name,
  control,
  validation,
}: DateRangeInputProps<TFieldValues>) {
  const {
    field: { value, onChange },
  } = useController({ control, name, rules: validation });
  return <DatePickerCalendar date={value} onDateChange={onChange} locale={cs} />;
}
