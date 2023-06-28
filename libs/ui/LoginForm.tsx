import { TextFieldElement } from '@app/ui/fields/text';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { useAuth } from '@app/ui/use-auth';
import Link, { LinkProps } from 'next/link';
import { useRouter } from 'next/router';
import * as React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';

type FormProps = {
  login: string;
  passwd: string;
};

type LoginFormProps = {
  successHref?: LinkProps['href'];
}

export function LoginForm(props: LoginFormProps) {
  const { signIn } = useAuth();
  const router = useRouter();
  const { control, handleSubmit } = useForm<FormProps>();

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await signIn(values.login, values.passwd);
    const redirect = router.query?.from as string | undefined;
    if (redirect) {
      return await router.push(redirect);
    }
    if (router.pathname === '/login' && props.successHref) {
      return await router.push(props.successHref);
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

          <div className="flex justify-between flex-wrap gap-2">
            <Link
              href="/register"
              className="uppercase rounded-md px-3 text-sm py-2 text-red-500 hover:bg-red-100 text-left"
            >
              Registrovat se
            </Link>
            <Link
              href="/forgotten-password"
              className="uppercase rounded-md px-3 text-sm py-2 text-red-500 hover:bg-red-100 text-right"
            >
              Zapomněli jste heslo?
            </Link>
          </div>
        </form>
      </div>
    </div>
  );
}
