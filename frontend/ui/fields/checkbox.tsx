import React from 'react';
import { type Control, type FieldValues, type Path, useController } from 'react-hook-form';
import { FieldHelper, type FieldHelperProps } from '@/ui/form';

type Extras = {
  label?: React.ReactNode;
};

function Checkbox({
  name,
  className,
  label,
  error,
  helperText,
  ...props
}: FieldHelperProps & Extras & Omit<React.HTMLProps<HTMLInputElement>, 'label'>) {
  return (
    <div className={`relative flex items-start my-1 ${className}`}>
      <div className="flex items-center h-5 pt-2 pl-0.5">
        <input
          id={name}
          name={name}
          type="checkbox"
          {...props}
          className="focus:ring-accent-9 size-4 bg-accent-2 text-accent-10 border-accent-9 border-2 rounded"
        />
      </div>
      <div className="ml-2 text-sm">
        <label htmlFor={name} className="block text-sm text-neutral-12 mt-1">
          {label}
        </label>
        <FieldHelper error={error} helperText={helperText} />
      </div>
    </div>
  );
}

type CheckboxElementProps<T extends FieldValues> = Omit<
  React.HTMLProps<HTMLInputElement>,
  'label' | 'name'
> & {
  name: Path<T>;
  control?: Control<T>;
} & Extras;

export function CheckboxElement<T extends FieldValues>({
  name,
  control,
  ...props
}: CheckboxElementProps<T>) {
  const { field, fieldState } = useController<T>({ name, control });

  return (
    <Checkbox
      name={name}
      value={field.value}
      checked={!!field.value}
      error={fieldState.error}
      {...props}
      onChange={() => field.onChange(!field.value)}
    />
  );
}
