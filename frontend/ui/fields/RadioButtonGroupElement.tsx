import React from 'react';
import {
  type Control,
  type FieldValues,
  type Path,
  useController,
} from 'react-hook-form';
import { FieldHelper, FieldLabel } from '@/ui/form';
import { CheckCircle, Circle } from 'lucide-react';
import { cn } from '@/lib/cn';

type Extras = {
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
};

export type RadioButtonGroupItem = {
  id: string;
  label: React.ReactNode;
  disabled?: boolean;
};

export function RadioButtonGroup({
  name,
  value,
  options,
  onValueChange,
  onBlur,
  className,
  itemClassName,
}: {
  name: string;
  value: string;
  options: RadioButtonGroupItem[];
  onValueChange: (value: string) => void;
  onBlur?: React.FocusEventHandler<HTMLInputElement>;
  className?: string;
  itemClassName?: string;
}) {
  return (
    <div className={cn('flex flex-wrap gap-1 rounded-xl', className)}>
      {options.map(({ label, id, disabled }) => (
        <label key={id} className="relative">
          <input
            className="peer sr-only"
            type="radio"
            name={name}
            value={id}
            checked={value === id}
            disabled={disabled}
            onBlur={onBlur}
            onChange={() => onValueChange(id)}
          />
          <span
            className={cn(
              'block cursor-pointer rounded-xl border border-accent-7 bg-neutral-1 px-2.5 py-2 text-sm text-accent-11',
              'peer-checked:border-accent-10 peer-checked:bg-accent-9 peer-checked:text-white',
              'peer-focus-visible:relative peer-focus-visible:z-30 peer-focus-visible:ring peer-focus-visible:ring-accent-10',
              'peer-disabled:cursor-not-allowed peer-disabled:border-neutral-6 peer-disabled:text-neutral-11',
              'peer-disabled:peer-checked:border-neutral-10 peer-disabled:peer-checked:bg-neutral-9 peer-disabled:peer-checked:text-white',
              itemClassName,
            )}
          >
            {label}
          </span>
        </label>
      ))}
    </div>
  );
}

type RadioButtonGroupElementProps<T extends FieldValues> = {
  name: Path<T>;
  control?: Control<T>;
  options: RadioButtonGroupItem[];
} & Extras;

export function RadioButtonGroupElement<T extends FieldValues>({
  name,
  control,
  options,
  className,
  helperText,
  label,
}: RadioButtonGroupElementProps<T>) {
  const { field, fieldState } = useController<T>({ name, control });

  return (
    <div className={cn('relative', className)}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <RadioButtonGroup
        name={field.name}
        value={field.value}
        options={options}
        onBlur={field.onBlur}
        onValueChange={field.onChange}
      />
      <FieldHelper error={fieldState.error} helperText={helperText} />
    </div>
  );
}

export function VerticalCheckboxButtonGroupElement<T extends FieldValues>({
  name,
  control,
  options,
  className,
  helperText,
  label,
}: RadioButtonGroupElementProps<T>) {
  const { field, fieldState } = useController<T>({ name, control });
  const selectedValues = Array.isArray(field.value) ? (field.value as string[]) : [];

  const handleChange = (id: string, checked: boolean) => {
    const currentValues = Array.isArray(field.value) ? (field.value as string[]) : [];
    if (checked) {
      field.onChange([...new Set([...currentValues, id])]);
      return;
    }
    field.onChange(currentValues.filter((value) => value !== id));
  };

  return (
    <div className={cn('relative', className)}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <div className="flex flex-col rounded-xl shadow-md">
        {options.map(({ label, id, disabled }, index) => (
          <label key={id} className="relative block">
            <input
              className="peer sr-only"
              type="checkbox"
              name={field.name}
              value={id}
              checked={selectedValues.includes(id)}
              disabled={disabled}
              onBlur={field.onBlur}
              onChange={(event) => handleChange(id, event.currentTarget.checked)}
            />
            <Circle className="pointer-events-none absolute left-2 top-1/2 z-10 size-4 -translate-y-1/2 text-accent-11 peer-checked:hidden peer-disabled:text-neutral-11" />
            <CheckCircle className="pointer-events-none absolute left-2 top-1/2 z-10 hidden size-4 -translate-y-1/2 text-white peer-checked:block peer-disabled:peer-checked:text-white" />
            <span
              className={cn(
                'block w-full cursor-pointer bg-neutral-1 py-2 pl-8 pr-2.5 text-sm text-accent-11',
                'border-x border-t border-accent-7',
                'peer-checked:border-accent-10 peer-checked:bg-accent-9 peer-checked:text-white',
                'peer-focus-visible:relative peer-focus-visible:z-30 peer-focus-visible:ring peer-focus-visible:ring-accent-10',
                'peer-disabled:cursor-not-allowed peer-disabled:border-neutral-6 peer-disabled:text-neutral-11',
                'peer-disabled:peer-checked:border-neutral-10 peer-disabled:peer-checked:bg-neutral-9 peer-disabled:peer-checked:text-white',
                index === 0 && 'rounded-t-xl',
                index === options.length - 1 && 'rounded-b-xl border-b',
              )}
            >
              {label}
            </span>
          </label>
        ))}
      </div>
      <FieldHelper error={fieldState.error} helperText={helperText} />
    </div>
  );
}
