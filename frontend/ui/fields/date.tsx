import * as React from 'react';
import { Control, FieldValues, Path, useController } from 'react-hook-form';
import cs from 'date-fns/locale/cs';
import cx from 'classnames';
import { ChevronLeft, ChevronRight, Calendar as CalendarIcon } from 'lucide-react';
import { DayPicker, DateRange } from 'react-day-picker';
import { FieldHelper, FieldLabel } from '@app/ui/form';
import { buttonCls } from '@app/ui/style';
import { TextField } from './text';
import { Popover, PopoverContent, PopoverTrigger } from '../popover';

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
          'p-1 border rounded-lg border-accent-9 bg-accent-1 space-y-4 sm:space-x-4 sm:space-y-0',
        month: 'space-y-4',
        caption: 'flex justify-center pt-1 mx-2 relative justify-between items-center',
        caption_label: 'text-sm font-medium',
        nav: 'gap-1 flex items-center',
        nav_button: buttonCls(),
        table: 'w-full border-collapse space-y-1',
        head_row: 'flex',
        head_cell: 'text-accent-11 rounded-md w-9 text-[0.8rem]',
        row: 'flex w-full mt-2',
        cell: 'text-center text-sm p-0 relative [&:has([aria-selected])]:bg-accent-3 first:[&:has([aria-selected])]:rounded-l-md last:[&:has([aria-selected])]:rounded-r-md focus-within:relative focus-within:z-30',
        day: 'flex justify-center items-center rounded-lg hover:bg-accent-4 h-9 w-9 p-0 aria-selected:opacity-100',
        day_selected:
          'bg-accent-9 text-accent-1 font-bold hover:!bg-accent-10 hover:text-accent-2 focus:bg-accent-10 focus:text-accent-2',
        day_today: 'bg-accent-4 text-accent-11',
        day_outside: 'text-neutral-11 opacity-50',
        day_disabled: 'text-neutral-11 opacity-50',
        day_range_middle: 'aria-selected:bg-accent-8 aria-selected:text-accent-1 aria-selected:font-normal',
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
  name: Path<T>;
  control?: Control<T>;
};

type Extras = {
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
};


export function DateRangeInput<T extends FieldValues>({
  name,
  control,
  label,
  className,
  helperText,
}: DateRangeInputProps<T> & Extras) {
  const { field, fieldState } = useController<T>({ control, name });

  const [month, setMonth] = React.useState(new Date());
  React.useEffect(() => {
    const m = field.value?.from || field.value?.to;
    if (m) setMonth(m);
  }, [field.value]);

  return (
    <div className={className}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <Calendar
        mode="range"
        defaultMonth={new Date()}
        month={month}
        onMonthChange={setMonth}
        selected={field.value}
        onSelect={field.onChange}
        locale={cs}
      />
      <FieldHelper error={fieldState.error} helperText={helperText} />
    </div>
  );
}

const toDatetimeLocal = (d: Date) => {
  const dt = new Date(d);
  dt.setMinutes(dt.getMinutes() - dt.getTimezoneOffset());
  return dt.toISOString().slice(0, 16);
};

export function DatePickerElement<T extends FieldValues>({
  name,
  control,
  label,
  className,
  helperText,
}: DateRangeInputProps<T> & Extras) {
  const { field, fieldState } = useController<T>({ control, name });

  const [input, setInput] = React.useState('');
  const [month, setMonth] = React.useState(new Date());

  React.useEffect(() => {
    const newInput = field.value ? toDatetimeLocal(new Date(field.value)) : '';
    setInput(old => old != newInput ? newInput : old);
  }, [field.value]);


  return (
    <div className={className}>
      <Popover>
        <FieldLabel htmlFor={name}>{label}</FieldLabel>
        <TextField
          prefix={
            <PopoverTrigger asChild>
              <button>
                <CalendarIcon className="text-accent-10" />
              </button>
            </PopoverTrigger>
          }
          type="datetime-local"
          name={name}
          value={input}
          error={fieldState.error}
          onChange={(e) => {
            const newInput = e.currentTarget.value;
            setInput(newInput);
            if (newInput) {
              const date = new Date(newInput);
              date.setMinutes(date.getMinutes() + date.getTimezoneOffset());
              if (!isNaN(date.valueOf())) {
                field.onChange(date);
                setMonth(date);
              }
            } else {
              field.onChange(null);
            }
          }}
        />
        <PopoverContent align="start">
          <Calendar
            mode="single"
            month={month}
            onMonthChange={setMonth}
            selected={field.value}
            onSelect={(date) => {
              field.onChange(date);
              setInput(toDatetimeLocal(new Date(field.value)));
            }}
            locale={cs}
          />
        </PopoverContent>
        <FieldHelper error={fieldState.error} helperText={helperText} />
      </Popover>
    </div>
  );
}
