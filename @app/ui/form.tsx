import { cn } from './cn';
import { FieldError, Path } from 'react-hook-form';

export const FieldLabel = ({
  className,
  children,
  ...props
}: React.HTMLAttributes<HTMLLabelElement> & { htmlFor?: string | Path<any> }) => {
  return (
    <label className={cn('block text-sm text-gray-700 mt-1', className)} {...props}>
      {children}
    </label>
  );
};

export type FieldHelperProps = {
  error?: FieldError;
  helperText?: React.ReactNode;
};

export const FieldHelper = ({ error, helperText }: FieldHelperProps) => {
  const parsedHelperText = !error ? helperText : error.message;
  if (!parsedHelperText) return null;
  return (
    <p className={cn('mt-2 text-sm', error ? 'text-red-800' : 'text-gray-500')}>
      {parsedHelperText}
    </p>
  );
};
