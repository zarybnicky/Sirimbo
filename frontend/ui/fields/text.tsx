import { cn } from '@/ui/cn';
import { FieldHelper, FieldHelperProps, FieldLabel } from '@/ui/form';
import { AlertCircle as ReportProblemIcon } from 'lucide-react';
import React from 'react';
import { Control, FieldValues, Path, useController } from 'react-hook-form';

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
}: FieldHelperProps & Extras & Omit<React.HTMLProps<HTMLInputElement>, 'label' | 'prefix'>) {
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
            "block w-full sm:text-sm rounded-md",
            "bg-accent-2 border-accent-7 text-accent-12 placeholder:text-accent-7",
            "disabled:bg-neutral-2 disabled:border-neutral-7 disabled:text-neutral-11 disabled:placeholder:text-neutral-9",
            "focus:outline-none focus:ring-accent-7 focus:border-accent-8",
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
  const valueAsNumber = props?.type === 'number';
  const { field, fieldState } = useController<T>({ name, control });

  return (
    <TextField
      name={name}
      value={field.value || ''}
      error={fieldState.error}
      {...props}
      onChange={(e) => field.onChange(valueAsNumber ? parseFloat(e.currentTarget.value) : e.currentTarget.value)}
    />
  );
}
