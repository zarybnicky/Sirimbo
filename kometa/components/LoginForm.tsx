import * as React from 'react';
import { useAuth } from 'lib/use-auth';
import { useRouter } from 'next/router';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from '@app/ui/fields/text';
import { FormError } from '@app/ui/form';
import { useAsyncCallback } from 'react-async-hook';
import { SubmitButton } from '@app/ui/submit';
import { Route } from 'nextjs-routes';

type FormProps = {
  login: string;
  passwd: string;
};

export function LoginForm() {
  const { signIn } = useAuth();
  const router = useRouter();
  const { control, handleSubmit } = useForm<FormProps>();

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await signIn(values.login, values.passwd);
    const redirect = router.query?.from as any as Route;
    if (redirect) {
      router.push(redirect);
    }
  });

  return (
    <div className="flex items-center justify-center min-h-[50vh]">
      <div className="group bg-white relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1">
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
        </form>
      </div>
    </div>
  );
};
