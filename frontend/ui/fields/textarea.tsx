import { FieldHelper, type FieldHelperProps, FieldLabel } from '@/ui/form';
import { AlertCircle as ReportProblemIcon } from 'lucide-react';
import React from 'react';
import { type Control, type FieldValues, type Path, useController } from 'react-hook-form';

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
      <div className="mt-1 relative rounded-md shadow-sm">
        <textarea
          id={name}
          name={name}
          {...props}
          className="bg-accent-3 text-accent-12 shadow-sm focus:ring-accent-7 focus:border-accent-8 block w-full sm:text-sm border-accent-7 rounded-md"
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
      onChange={(e) => field.onChange(e.currentTarget.value)}
    />
  );
}
