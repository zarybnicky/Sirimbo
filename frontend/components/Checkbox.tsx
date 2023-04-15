import {
  Control,
  FieldValues,
  ControllerProps,
  FieldError,
  Path,
  useController,
} from 'react-hook-form';

type Extras = {
  label?: React.ReactNode;
  helperText?: React.ReactNode;
  parseError?: (error: FieldError) => React.ReactNode;
};

type CheckboxElementProps<T extends FieldValues> = Omit<
  React.HTMLProps<HTMLInputElement>,
  'label' | 'name'
> & {
  validation?: ControllerProps['rules'];
  name: Path<T>;
  control?: Control<T>;
} & Extras;

export function Checkbox({
  name,
  className,
  label,
  error,
  helperText,
  parseError,
  required,
  ...props
}: {
  error?: FieldError;
} & Extras &
  Omit<React.HTMLProps<HTMLInputElement>, 'label'>) {
  const parsedHelperText = !error
    ? helperText
    : parseError
    ? parseError(error)
    : error.message;
  return (
    <div className={`relative flex items-start my-2 ${className}`}>
      <div className="flex items-center h-5">
        <input
          id={name}
          name={name}
          type="checkbox"
          {...props}
          className="focus:ring-red-500 h-4 w-4 text-red-600 border-gray-300 rounded"
        />
      </div>
      <div className="ml-3 text-sm">
        <label htmlFor={name} className="font-medium text-gray-700">
          {label}
          {required && <sup>*</sup>}
        </label>
        {parsedHelperText && (
          <p className={error ? 'text-red-800' : 'text-gray-500'}>{parsedHelperText}</p>
        )}
      </div>
    </div>
  );
}

export function CheckboxElement<TFieldValues extends FieldValues>({
  name,
  required,
  control,
  validation = {},
  ...props
}: CheckboxElementProps<TFieldValues>) {
  if (required && !validation?.required) {
    validation.required = 'Toto pole je povinné';
  }
  const {
    field: { value, onChange },
    fieldState: { error },
  } = useController({
    name,
    rules: validation,
    control,
  });

  return (
    <Checkbox
      name={name}
      value={value}
      checked={!!value}
      error={error}
      {...props}
      onChange={() => onChange(!value)}
    />
  );
}
