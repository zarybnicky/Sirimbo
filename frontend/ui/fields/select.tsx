import { cn } from '@/lib/cn';
import { FieldHelper, type FieldHelperProps, FieldLabel } from '@/ui/form';
import React from 'react';

export type SelectOption<Value extends string = string> = {
  value: Value;
  label: React.ReactNode;
};

export type SelectFieldProps<Value extends string = string> = Omit<
  React.SelectHTMLAttributes<HTMLSelectElement>,
  'value' | 'onChange'
> &
  FieldHelperProps & {
    value: Value;
    onChange: (value: Value) => void;
    options: readonly SelectOption<Value>[];
    label?: React.ReactNode;
    placeholder?: React.ReactNode;
    className?: string;
    selectClassName?: string;
  };

export function SelectField<Value extends string = string>({
  name,
  value,
  onChange,
  options,
  label,
  placeholder,
  error,
  helperText,
  className,
  selectClassName,
  ...props
}: SelectFieldProps<Value>) {
  return (
    <div className={className || ''}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <select
        id={name}
        name={name}
        value={value}
        onChange={(event) => onChange(event.currentTarget.value as Value)}
        {...props}
        className={cn(
          'block w-full min-w-0 rounded-md',
          'bg-accent-2 border-accent-7 text-accent-12',
          'py-2 pl-3 pr-8 text-sm',
          'disabled:bg-neutral-2 disabled:border-neutral-7 disabled:text-neutral-11',
          'focus:outline-none focus:ring-accent-7 focus:border-accent-8',
          selectClassName,
        )}
      >
        {placeholder ? <option value="">{placeholder}</option> : null}
        {options.map((option) => (
          <option key={option.value} value={option.value}>
            {option.label}
          </option>
        ))}
      </select>
      <FieldHelper error={error} helperText={helperText} />
    </div>
  );
}
