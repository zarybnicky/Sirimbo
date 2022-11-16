import * as ToggleGroupPrimitive from "@radix-ui/react-toggle-group";
import classNames from "classnames";
import React from "react";
import { useController, FieldError, FieldValues, Path, Control, ControllerProps } from 'react-hook-form';

type Extras = {
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
  parseError?: (error: FieldError) => React.ReactNode;
};

type Item = { id: string; label: string; disabled?: boolean; };
export type RadioButtonGroupElementProps<T extends FieldValues> = {
  validation?: ControllerProps['rules'];
  name: Path<T>;
  control?: Control<T>;
  options: Item[];
  required?: boolean;
} & Extras;

export function RadioButtonGroupElement<TFieldValues extends FieldValues>({
  name, control, validation = {}, required, options, className, helperText, parseError, label,
}: RadioButtonGroupElementProps<TFieldValues>) {
  if (required && !validation?.required) {
    validation.required = 'Toto pole je povinné';
  }

  const { field: { value, onChange }, fieldState: { error } } = useController({
    name, control, rules: validation,
  });
  const parsedHelperText = !error ? helperText : parseError ? parseError(error) : error.message;

  return <div className={`relative my-2 ${className}`}>
    <div className="text-slate-700 text-sm mb-1">{label}</div>
    <ToggleGroupPrimitive.Root value={value} onValueChange={onChange} type="single">
      {options.map(({ label, id, disabled }) => (
        <ToggleGroupPrimitive.Item
          key={`group-item-${id}-${label}`}
          value={id}
          disabled={disabled}
          className={classNames(
            "group radix-state-on:text-white radix-state-on:bg-red-500 bg-white",
            "border-y px-2.5 py-2 first:rounded-l-xl first:border-x last:rounded-r-xl last:border-x",
            "border-gray-400 radix-state-on:border-red-800",
            "focus:relative focus:outline-none focus-visible:z-20 focus-visible:ring focus-visible:ring-purple-500 focus-visible:ring-opacity-75"
          )}
        >
          {label}
        </ToggleGroupPrimitive.Item>
      ))}
    </ToggleGroupPrimitive.Root>
    {parsedHelperText && (
      <p className={classNames("mt-2 text-sm", error ? 'text-red-600' : 'text-gray-500')}>{parsedHelperText}</p>
    )}
  </div>;
};
