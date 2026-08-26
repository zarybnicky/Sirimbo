import React from 'react';
import {
  type Control,
  type FieldValues,
  type Path,
  useController,
} from 'react-hook-form';
import { FieldHelper, type FieldHelperProps } from '@/ui/form';
import { checkboxInputCls } from '@/ui/style';
import { cn } from '@/lib/cn';

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
    <div className={cn('relative my-1 flex items-start', className)}>
      <div className="flex items-center h-5 pt-2 pl-0.5">
        <input
          id={name}
          name={name}
          type="checkbox"
          {...props}
          className={checkboxInputCls()}
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
      onBlur={field.onBlur}
      onChange={() => {
        field.onChange(!field.value);
        field.onBlur();
      }}
    />
  );
}
