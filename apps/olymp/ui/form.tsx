import React from 'react';
import { cn } from './cn';
import { FieldError, Path } from 'react-hook-form';

export const FieldLabel = ({
  className,
  children,
  ...props
}: React.HTMLAttributes<HTMLLabelElement> & { htmlFor?: string | Path<unknown> }) => {
  if (!children) return null;
  return (
    <label className={cn('block text-sm text-neutral-11 mt-1', className)} {...props}>
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
    <p className={cn('mt-2 text-sm', error ? 'text-accent-11' : 'text-neutral-10')}>
      {parsedHelperText}
    </p>
  );
};

const errorTranslation: { [key: string]: string } = {
  ACCOUNT_NOT_FOUND: 'Zadaná kombinace jména a e-mailu neexistuje',
  INVALID_PASSWORD: 'Nesprávné heslo',
  ACCOUNT_DISABLED: 'Učet byl zablokován',
  ACCOUNT_NOT_CONFIRMED: 'Účet ještě nebyl potvrzen',
};

export const FormError = ({ error: e, default: def }: { error: unknown; default?: React.ReactNode }) => {
  let error: string | null = null;
  if (!e) {
    return null;
  }
  if (e instanceof Error || (typeof e === 'object' && 'message' in e)) {
    error = (e as any).message;
  } else if (e) {
    error = e.toString();
  }
  if (!error) {
    return null;
  }

  return (
    <div className="rounded-lg px-4 py-2 bg-accent-10 text-accent-1 col-full">
      {errorTranslation[error] || (
        <>
          <div className="font-bold">
            {def || 'Něco se nepovedlo, zkuste to prosím znovu'}
          </div>
          <div className="text-sm">{error}</div>
        </>
      )}
    </div>
  );
};