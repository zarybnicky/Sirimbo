import React from 'react';
import { AlertCircle as ReportProblemIcon } from 'lucide-react';
import {
  Control,
  FieldValues,
  ControllerProps,
  Path,
  useController,
} from 'react-hook-form';
import { FieldHelper, FieldHelperProps, FieldLabel } from '@app/ui/form';

type Extras = {
  className?: string;
  label?: React.ReactNode;
  helperText?: string;
};

export type TextFieldElementProps<T extends FieldValues> = Omit<
  React.HTMLProps<HTMLInputElement>,
  'label' | 'name'
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
  ...props
}: FieldHelperProps & Extras & Omit<React.HTMLProps<HTMLInputElement>, 'label'>) {
  return (
    <div className={className || ''}>
      <FieldLabel htmlFor={name}>{label}</FieldLabel>
      <div className="relative rounded-md shadow-sm">
        <input
          id={name}
          name={name}
          type={type}
          {...props}
          className="block w-full border-red-400 text-stone-900 placeholder:text-red-300 focus:outline-none focus:ring-red-500 focus:border-red-500 sm:text-sm rounded-md"
        />
        {error && (
          <div className="absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none">
            <ReportProblemIcon className="h-5 w-5 text-red-500" aria-hidden="true" />
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
      onChange={(e) =>
        field.onChange(
          valueAsNumber ? parseInt(e.currentTarget.value, 10) : e.currentTarget.value,
        )
      }
    />
  );
}
