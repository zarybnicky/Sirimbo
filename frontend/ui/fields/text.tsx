import {
  FieldErrorIcon,
  FieldHelper,
  type FieldHelperProps,
  FieldLabel,
} from '@/ui/form';
import { inputCls, inputGroupCls } from '@/ui/style';
import React from 'react';
import {
  type Control,
  type FieldValues,
  type Path,
  useController,
} from 'react-hook-form';

type Extras = {
  className?: string;
  inputClassName?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
  prefix?: React.ReactNode;
};

export function InputGroup({
  className,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) {
  return <div className={inputGroupCls({ className })} {...props} />;
}

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
  inputClassName,
  label,
  error,
  helperText,
  prefix,
  ...props
}: FieldHelperProps &
  Extras &
  Omit<React.InputHTMLAttributes<HTMLInputElement>, 'label' | 'prefix'>) {
  return (
    <div className={className || ''}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <div className="flex gap-2 relative rounded-md shadow-xs">
        {prefix}
        <input
          id={name}
          name={name}
          type={type}
          {...props}
          className={inputCls({ className: inputClassName })}
        />
        {error && <FieldErrorIcon />}
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
