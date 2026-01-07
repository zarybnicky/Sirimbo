import { cn } from '@/ui/cn';
import { FieldHelper, type FieldHelperProps, FieldLabel } from '@/ui/form';
import { AlertCircle as ReportProblemIcon } from 'lucide-react';
import React from 'react';
import {
  type Control,
  type FieldValues,
  type Path,
  useController,
} from 'react-hook-form';

type Extras = {
  className?: string;
  label?: React.ReactNode;
  helperText?: string;
  prefix?: React.ReactNode;
};

export type TextFieldElementProps<T extends FieldValues> = Omit<
  React.HTMLProps<HTMLInputElement>,
  'label' | 'name' | 'prefix'
> & {
  name: Path<T>;
  control?: Control<T>;
} & Extras;

export function TextField({
  name,
  type = 'text',
  className,
  label,
  error,
  helperText,
  prefix,
  ...props
}: FieldHelperProps &
  Extras &
  Omit<React.HTMLProps<HTMLInputElement>, 'label' | 'prefix'>) {
  return (
    <div className={className || ''}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <div className="flex gap-2 relative rounded-md shadow-sm">
        {prefix}
        <input
          id={name}
          name={name}
          type={type}
          {...props}
          className={cn(
            'block w-full sm:text-sm rounded-md',
            'bg-accent-2 border-accent-7 text-accent-12 placeholder:text-accent-7',
            'disabled:bg-neutral-2 disabled:border-neutral-7 disabled:text-neutral-11 disabled:placeholder:text-neutral-9',
            'read-only:bg-neutral-2 read-only:border-neutral-7 read-only:text-neutral-11 read-only:placeholder:text-neutral-9',
            'focus:outline-none focus:ring-accent-7 focus:border-accent-8',
          )}
        />
        {error && (
          <div className="absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none">
            <ReportProblemIcon className="size-5 text-accent-7" aria-hidden="true" />
          </div>
        )}
      </div>
      <FieldHelper error={error} helperText={helperText} />
    </div>
  );
}

export function TextFieldElement<T extends FieldValues>({
  name,
  control,
  ...props
}: TextFieldElementProps<T>) {
  const {
    onBlur: onBlurProp,
    onChange: onChangeProp,
    type = 'text',
    ...restProps
  } = props;
  const valueAsNumber = type === 'number';
  const { field, fieldState } = useController<T>({ name, control });

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    onChangeProp?.(event);

    const nextValue = event.currentTarget.value;
    if (!valueAsNumber) {
      field.onChange(nextValue);
      return;
    }

    if (nextValue === '') {
      field.onChange();
      return;
    }

    const parsedValue = Number.parseFloat(nextValue);
    field.onChange(Number.isNaN(parsedValue) ? undefined : parsedValue);
  };

  const handleBlur = (event: React.FocusEvent<HTMLInputElement>) => {
    onBlurProp?.(event);
    field.onBlur();
  };

  return (
    <TextField
      name={name}
      type={type}
      value={field.value ?? ''}
      error={fieldState.error}
      {...restProps}
      onBlur={handleBlur}
      onChange={handleChange}
    />
  );
}
