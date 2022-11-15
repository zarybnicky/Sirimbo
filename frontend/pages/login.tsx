import * as React from 'react';
import { Card } from 'components/Card';
import Link from 'next/link';
import { useAuth } from 'lib/data/use-auth';
import { useRouter } from 'next/router';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useRequireUserLoggedOut } from 'lib/route-guards';
import { ErrorBox } from 'components/ErrorBox';
import { useAsyncCallback } from 'react-async-hook';
import { SubmitButton } from 'components/SubmitButton';

type FormProps = {
  login: string;
  passwd: string;
};

export default function LoginPage() {
  useRequireUserLoggedOut();
  const { signIn } = useAuth();
  const router = useRouter();
  const { control, handleSubmit, formState } = useForm<FormProps>();

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await signIn(values.login, values.passwd);
    router.push(router.query?.from as string || '/dashboard');
  });

  return (
    <div className="container mx-auto max-w-md mt-12 mb-16">
      <Card>
        <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
          <h4>Přihlášení do systému</h4>

          <ErrorBox error={onSubmit.error} />
          <TextFieldElement
            control={control} name="login"
            label="E-mail nebo přihlašovací jméno" autoComplete="username"
            required autoFocus
          />
          <TextFieldElement
            control={control} name="passwd" type="password"
            label="Heslo" autoComplete="current-password" required
          />
          <SubmitButton className="w-full my-2" loading={onSubmit.loading} disabled={!formState.isValid}>
            Přihlásit
          </SubmitButton>

          <div className="grid grid-cols-2 gap-2">
            <Link href="/register" passHref>
              <a className="button button-red button-text">
                Registrovat se
              </a>
            </Link>
            <Link href="/forgotten-password" passHref>
              <a className="button button-red button-text text-right">
                Zapomněli jste heslo?
              </a>
            </Link>
          </div>
        </form>
      </Card>
    </div>
  );
};
