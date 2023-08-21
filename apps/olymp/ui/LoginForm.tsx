import { TextFieldElement } from '@app/ui/fields/text';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { useAuth } from '@app/ui/use-auth';
import Link from 'next/link';
import * as React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';

type FormProps = {
  login: string;
  passwd: string;
};

type LoginFormProps = {
  onSuccess?: () => void;
}

export function LoginForm({ onSuccess }: LoginFormProps) {
  const { signIn } = useAuth();
  const { control, handleSubmit } = useForm<FormProps>();

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await signIn(values.login, values.passwd);
    onSuccess?.();
  });

  return (
    <div className="flex items-center justify-center min-h-[60vh]">
      <div className="group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1">
        <form className="grid gap-2 p-4" onSubmit={handleSubmit(onSubmit.execute)}>
          <h4 className="text-2xl">Přihlášení do systému</h4>

          <FormError error={onSubmit.error} />
          <TextFieldElement
            control={control}
            name="login"
            label="E-mail nebo přihlašovací jméno"
            autoComplete="username"
            required
            autoFocus
          />
          <TextFieldElement
            control={control}
            name="passwd"
            type="password"
            label="Heslo"
            autoComplete="current-password"
            required
          />
          <SubmitButton className="w-full my-2" loading={onSubmit.loading}>
            Přihlásit
          </SubmitButton>

          <div className="flex justify-between flex-wrap gap-2">
            {/* <Link
              href="/registrace"
              className="uppercase rounded-md px-3 text-sm py-2 text-red-500 hover:bg-red-100 text-left"
            >
              Registrovat se
            </Link> */}
            <Link
              href="/zapomenute-heslo"
              className="uppercase rounded-md px-3 text-sm py-2 text-accent-10 hover:bg-accent-3 text-right"
            >
              Zapomněli jste heslo?
            </Link>
          </div>
        </form>
      </div>
    </div>
  );
}
