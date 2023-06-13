import * as ToggleGroupPrimitive from '@radix-ui/react-toggle-group';
import classNames from 'classnames';
import React from 'react';
import {
  useController,
  FieldValues,
  Path,
  Control,
  ControllerProps,
} from 'react-hook-form';
import { FieldHelper, FieldLabel } from './ui/form';

type Extras = {
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
};

type Item = { id: string; label: string; disabled?: boolean };
export type RadioButtonGroupElementProps<T extends FieldValues> = {
  validation?: ControllerProps['rules'];
  name: Path<T>;
  control?: Control<T>;
  options: Item[];
  required?: boolean;
} & Extras;

export function RadioButtonGroupElement<T extends FieldValues>({
  name,
  control,
  validation = {},
  required,
  options,
  className,
  helperText,
  label,
}: RadioButtonGroupElementProps<T>) {
  if (required && !validation?.required) {
    validation.required = 'Toto pole je povinné';
  }

  const { field, fieldState } = useController<T>({ name, control, rules: validation });

  return (
    <div className={`relative my-2 ${className}`}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <ToggleGroupPrimitive.Root
        value={field.value}
        onValueChange={field.onChange}
        type="single"
      >
        {options.map(({ label, id, disabled }) => (
          <ToggleGroupPrimitive.Item
            key={`group-item-${id}-${label}`}
            value={id}
            disabled={disabled}
            className={classNames(
              'group data-[state=on]:text-white data-[state=on]:bg-red-500 bg-white',
              'border-y px-2.5 py-2 first:rounded-l-xl first:border-x last:rounded-r-xl last:border-x',
              'border-gray-400 data-[state=on]:border-red-800',
              'focus:relative focus:outline-none focus-visible:z-20 focus-visible:ring focus-visible:ring-red-500 focus-visible:ring-opacity-75',
            )}
          >
            {label}
          </ToggleGroupPrimitive.Item>
        ))}
      </ToggleGroupPrimitive.Root>
      <FieldHelper error={fieldState.error} helperText={helperText} />
    </div>
  );
}
