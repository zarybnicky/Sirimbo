import {
  FieldErrorIcon,
  FieldHelper,
  type FieldHelperProps,
  FieldLabel,
} from '@/ui/form';
import { inputCls } from '@/ui/style';
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
};

type TextAreaElementProps<T extends FieldValues> = Omit<
  React.HTMLProps<HTMLTextAreaElement>,
  'label' | 'name' | 'control'
> & {
  name: Path<T>;
  control?: Control<T>;
} & Extras;

function TextArea({
  name,
  label,
  className,
  error,
  helperText,
  ...props
}: FieldHelperProps & Extras & Omit<React.HTMLProps<HTMLTextAreaElement>, 'label'>) {
  return (
    <div className={className}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <div className="relative rounded-md shadow-xs">
        <textarea id={name} name={name} {...props} className={inputCls()} />
        {error && <FieldErrorIcon />}
      </div>
      <FieldHelper error={error} helperText={helperText} />
    </div>
  );
}

export function TextAreaElement<T extends FieldValues>({
  name,
  control,
  ...props
}: TextAreaElementProps<T>) {
  const { field, fieldState } = useController<T>({ name, control });

  return (
    <TextArea
      name={name}
      value={field.value}
      error={fieldState.error}
      {...props}
      onBlur={(event) => {
        props.onBlur?.(event);
        field.onBlur();
      }}
      onChange={(e) => field.onChange(e.currentTarget.value)}
    />
  );
}
