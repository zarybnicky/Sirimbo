import classNames from 'classnames';
import ReportProblemIcon from '@mui/icons-material/ReportProblem';
import { Control, FieldValues, ControllerProps, FieldError, Path, useController } from 'react-hook-form';

type Extras = {
  className?: string;
  label?: React.ReactNode;
  helperText?: React.ReactNode;
  parseError?: (error: FieldError) => React.ReactNode;
};

export type TextAreaElementProps<T extends FieldValues> = Omit<React.HTMLProps<HTMLTextAreaElement>, 'label' | 'name'> & {
  validation?: ControllerProps['rules'];
  name: Path<T>;
  control?: Control<T>;
} & Extras;

export function TextArea({ name, label, className, error, helperText, parseError, required, ...props }: {
  error?: FieldError;
} & Extras & Omit<React.HTMLProps<HTMLTextAreaElement>, 'label'>) {
  const parsedHelperText = !error ? helperText : parseError ? parseError(error) : error.message;

  return (
    <div className={className}>
      <label htmlFor={name} className="block text-sm font-medium text-gray-700">
        {label}
      </label>
      <div className="mt-1 relative rounded-md shadow-sm">
        <textarea
          id={name} name={name} {...props}
          className="text-slate-900 shadow-sm focus:ring-red-500 focus:border-red-500 block w-full sm:text-sm border-red-400 rounded-md"
        />
        {error && (
          <div className="absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none">
            <ReportProblemIcon className="h-5 w-5 text-red-500" aria-hidden="true" />
          </div>
        )}
      </div>
      {parsedHelperText && (
        <p className={classNames("mt-2 text-sm", error ? 'text-red-600' : 'text-gray-500')}>{parsedHelperText}</p>
      )}
    </div>
  );
};

export type TextFieldElementProps<T extends FieldValues> = Omit<React.HTMLProps<HTMLInputElement>, 'label' | 'name'> & {
  validation?: ControllerProps['rules'];
  name: Path<T>;
  control?: Control<T>
} & Extras;

export function TextField({ name, type = "text", label, error, helperText, parseError, required, ...props }: {
  error?: FieldError;
} & Extras & Omit<React.HTMLProps<HTMLInputElement>, 'label'>) {
  const parsedHelperText = !error ? helperText : parseError ? parseError(error) : error.message;

  return (
    <div>
      <label htmlFor={name} className="block text-sm font-medium text-slate-700 mt-1 mb-1">
        {label}
      </label>
      <div className="relative rounded-md shadow-sm">
        <input
          id={name} name={name} type={type} {...props}
          className="block w-full pr-10 border-red-400 text-slate-900 placeholder-red-300 focus:outline-none focus:ring-red-500 focus:border-red-500 sm:text-sm rounded-md"
        />
        {error && (
          <div className="absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none">
            <ReportProblemIcon className="h-5 w-5 text-red-500" aria-hidden="true" />
          </div>
        )}
      </div>
      {parsedHelperText && (
        <p className={classNames("mt-2 text-sm", error ? 'text-red-600' : 'text-gray-500')}>{parsedHelperText}</p>
      )}
    </div>
  );
};

export function TextFieldElement<TFieldValues extends FieldValues>({
  name, required, control, validation = {}, ...props
}: TextFieldElementProps<TFieldValues>) {
  if (required && !validation?.required) {
    validation.required = 'Toto pole je povinné';
  }

  if (props?.type === 'email' && !validation.pattern) {
    validation.pattern = {
      value: /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/,
      message: 'Zadejte platný e-mail',
    };
  }
  const { field: { value, onChange }, fieldState: { error } } = useController({
    name, control, rules: validation
  });

  return <TextField
    name={name} value={value} error={error} {...props}
    onChange={(e) => onChange(e.currentTarget.value)}
  />;
};

export function TextAreaElement<TFieldValues extends FieldValues>({
  name, required, control, validation = {}, ...props
}: TextAreaElementProps<TFieldValues>) {
  if (required && !validation?.required) {
    validation.required = 'Toto pole je povinné';
  }
  const { field: { value, onChange }, fieldState: { error } } = useController({
    name, control, rules: validation
  });

  return <TextArea
    name={name} value={value} error={error} {...props}
    onChange={(e) => onChange(e.currentTarget.value)}
  />;
};
