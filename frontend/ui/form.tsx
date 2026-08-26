import React from 'react';
import { cn } from '@/lib/cn';
import type { FieldError, Path } from 'react-hook-form';
import { typographyCls } from '@/ui/style';
import { AlertCircle } from 'lucide-react';

interface FormResultContext {
  onSuccess: () => void;
}
export const FormResultContext = React.createContext<FormResultContext>({
  onSuccess() {},
});

export const useFormResult = () => React.useContext(FormResultContext);

export function FieldLabel({
  className,
  children,
  ...props
}: React.HTMLAttributes<HTMLLabelElement> & { htmlFor?: string | Path<unknown> }) {
  if (!children) return null;
  return (
    <label className={typographyCls({ variant: 'label', className })} {...props}>
      {children}
    </label>
  );
}

export type FieldHelperProps = {
  error?: FieldError;
  helperText?: React.ReactNode;
};

export function FieldErrorIcon() {
  return (
    <div className="pointer-events-none absolute inset-y-0 right-0 flex items-center pr-3">
      <AlertCircle className="size-5 text-danger-11" aria-hidden="true" />
    </div>
  );
}

export function FieldHelper({ error, helperText }: FieldHelperProps) {
  const parsedHelperText = !error ? helperText : error.message;
  if (!parsedHelperText) return null;
  return (
    <p className={cn('mt-2 text-sm', error ? 'text-danger-11' : 'text-neutral-10')}>
      {parsedHelperText}
    </p>
  );
}

const errorTranslation: { [key: string]: string } = {
  INVALID_CREDENTIALS: 'Nesprávné jméno nebo heslo',
  ACCOUNT_NOT_FOUND: 'Zadaná kombinace jména a e-mailu neexistuje',
  INVALID_PASSWORD: 'Nesprávné heslo',
  ACCOUNT_DISABLED: 'Účet byl zablokován',
  ACCOUNT_NOT_CONFIRMED: 'Účet ještě nebyl potvrzen',
  INVITATION_NOT_FOUND: 'Pozvánka není platná',
  INVITATION_ALREADY_USED: 'Pozvánka již byla použita',
  INVALID_EMAIL: 'E-mail neodpovídá pozvánce',
};

export function FormError({
  error: e,
  default: def,
}: {
  error: React.ReactNode | Error;
  default?: React.ReactNode;
}) {
  if (!e || (typeof e === 'object' && Object.keys(e).length === 0)) {
    return null;
  }
  let error: React.ReactNode | null;
  if (e instanceof Error || (typeof e === 'object' && 'message' in e)) {
    error = (e as any).message;
  } else {
    error = e;
  }
  if (!error) {
    return null;
  }

  return (
    <div className="col-full rounded-lg bg-danger-9 px-4 py-2 text-white">
      {errorTranslation[error as any] || (
        <>
          <div className="font-bold">
            {def || 'Něco se nepovedlo, zkuste to prosím znovu'}
          </div>
          <div className="text-sm">{error}</div>
        </>
      )}
    </div>
  );
}
