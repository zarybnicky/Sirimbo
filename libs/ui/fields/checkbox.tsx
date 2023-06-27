import React from 'react';
import { Control, FieldValues, Path, useController } from 'react-hook-form';
import { FieldHelper, FieldHelperProps } from '@app/ui/form';

type Extras = {
  label?: React.ReactNode;
};

export function Checkbox({
  name,
  className,
  label,
  error,
  helperText,
  ...props
}: FieldHelperProps & Extras & Omit<React.HTMLProps<HTMLInputElement>, 'label'>) {
  return (
    <div className={`relative flex items-start my-2 ${className}`}>
      <div className="flex items-center h-5 pt-2 pl-0.5">
        <input
          id={name}
          name={name}
          type="checkbox"
          {...props}
          className="focus:ring-accent-9 h-4 w-4 bg-accent-2 text-accent-10 border-accent-9 border-[2px] rounded"
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
