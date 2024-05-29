import { LoginDocument, UserAuthFragment } from '@/graphql/CurrentUser';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import Link from 'next/link';
import * as React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { useMutation } from 'urql';

type FormProps = {
  login: string;
  passwd: string;
};

type LoginFormProps = {
  onSuccess?: (result: UserAuthFragment | null) => void;
}

export function LoginForm({ onSuccess }: LoginFormProps) {
  const { control, handleSubmit } = useForm<FormProps>();
  const doSignIn = useMutation(LoginDocument)[1];

  const onSubmit = useAsyncCallback(async ({ login, passwd }: FormProps) => {
    const result = await doSignIn({ login, passwd });
    onSuccess?.(result.data?.login?.result?.usr ?? null);
  });

  return (
    <div className="flex h-[calc(100vh-80px)] items-center justify-center p-5 bg-neutral-1 w-full">
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
            <Link
              href="/frontend/pages/registrace"
              className="uppercase rounded-md px-3 text-sm py-2 text-accent-10 hover:bg-accent-3 text-left"
            >
              Přihlásit nového člena
            </Link>
            <Link
              href="/frontend/pages/zapomenute-heslo"
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
