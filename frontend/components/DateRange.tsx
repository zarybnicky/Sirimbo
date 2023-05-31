import * as React from 'react';
import {
  Control,
  FieldValues,
  ControllerProps,
  Path,
  useController,
  FieldError,
} from 'react-hook-form';
import cs from 'date-fns/locale/cs';
import cx from 'classnames';
import { ChevronLeft, ChevronRight } from 'lucide-react';
import { DayPicker, DateRange } from 'react-day-picker';

export type { DateRange };

export function Calendar({
  className,
  classNames,
  showOutsideDays = true,
  ...props
}: React.ComponentProps<typeof DayPicker>) {
  return (
    <DayPicker
      showOutsideDays={showOutsideDays}
      className={cx('py-2 flex flex-row', className)}
      classNames={{
        months:
          'p-1 border rounded-lg border-red-500 bg-white space-y-4 sm:space-x-4 sm:space-y-0',
        month: 'space-y-4',
        caption: 'flex justify-center pt-1 mx-2 relative justify-between items-center',
        caption_label: 'text-sm font-medium',
        nav: 'gap-1 flex items-center',
        nav_button: 'button button-red opacity-80 hover:opacity-100',
        table: 'w-full border-collapse space-y-1',
        head_row: 'flex',
        head_cell: 'text-stone-700 rounded-md w-9 text-[0.8rem]',
        row: 'flex w-full mt-2',
        cell: 'text-center text-sm p-0 relative [&:has([aria-selected])]:bg-red-100 first:[&:has([aria-selected])]:rounded-l-md last:[&:has([aria-selected])]:rounded-r-md focus-within:relative focus-within:z-20',
        day: 'flex justify-center items-center rounded-lg hover:bg-stone-200 h-9 w-9 p-0 aria-selected:opacity-100',
        day_selected:
          'bg-red-500 text-white font-bold hover:!bg-red-500 hover:text-white focus:bg-red-500 focus:text-white',
        day_today: 'bg-stone-200 text-black',
        day_outside: 'text-stone-700 opacity-50',
        day_disabled: 'text-stone-700 opacity-50',
        day_range_middle:
          'aria-selected:bg-red-100 aria-selected:text-stone-700 aria-selected:font-normal',
        day_hidden: 'invisible',
        ...classNames,
      }}
      components={{
        IconLeft: () => <ChevronLeft className="h-4 w-4" />,
        IconRight: () => <ChevronRight className="h-4 w-4" />,
      }}
      {...props}
    />
  );
}

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
  const { field, fieldState } = useController({ control, name, rules: validation });

  const [month, setMonth] = React.useState(new Date());
  React.useEffect(() => {
    const m = field.value?.from || field.value?.to;
    if (m) setMonth(m);
    console.log(field.value)
  }, [field.value]);

  const parsedHelperText = !fieldState.error
    ? helperText
    : parseError
    ? parseError(fieldState.error)
    : fieldState.error.message;

  return (
    <div className={className}>
      <label htmlFor={name} className="block text-sm text-gray-700 mt-1">
        {label}
      </label>

      <Calendar
        mode="range"
        defaultMonth={new Date()}
        month={month}
        onMonthChange={setMonth}
        selected={field.value}
        onSelect={field.onChange}
        locale={cs}
      />
      {parsedHelperText && (
        <p
          className={cx(
            'mt-2 text-sm',
            fieldState.error ? 'text-red-600' : 'text-gray-500',
          )}
        >
          {parsedHelperText}
        </p>
      )}
    </div>
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
  const { field, fieldState } = useController({ control, name, rules: validation });
  const parsedHelperText = !fieldState.error
    ? helperText
    : parseError
    ? parseError(fieldState.error)
    : fieldState.error.message;

  const [month, setMonth] = React.useState(new Date());
  React.useEffect(() => {
    setMonth(field.value);
  }, [field.value]);

  return (
    <div className={className}>
      <label htmlFor={name} className="block text-sm text-gray-700 my-1">
        {label}
      </label>
      <Calendar
        mode="single"
        month={month}
        onMonthChange={setMonth}
        selected={field.value}
        onSelect={field.onChange}
        locale={cs}
      />
      {parsedHelperText && (
        <p
          className={cx(
            'mt-2 text-sm',
            fieldState.error ? 'text-red-600' : 'text-gray-500',
          )}
        >
          {parsedHelperText}
        </p>
      )}
    </div>
  );
}
