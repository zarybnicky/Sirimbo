import * as React from 'react';
import { Card } from 'components/Card';
import Link from 'next/link';
import { useAuth } from 'lib/data/use-auth';
import { useRouter } from 'next/router';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { ErrorBox } from 'components/ErrorBox';
import { useAsyncCallback } from 'react-async-hook';
import { SubmitButton } from 'components/SubmitButton';
import { withServerLoggedOut } from 'lib/data/use-server-permissions';

type FormProps = {
  login: string;
  passwd: string;
};

export default function LoginPage() {
  const { signIn } = useAuth();
  const router = useRouter();
  const { control, handleSubmit } = useForm<FormProps>();

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await signIn(values.login, values.passwd);
    router.push(router.query?.from as string || '/dashboard');
  });

  return (
    <div className="flex items-center justify-center min-h-[50vh]">
      <Card>
        <form className="grid gap-2 p-4" onSubmit={handleSubmit(onSubmit.execute)}>
          <h4 className="text-2xl">Přihlášení do systému</h4>

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
          <SubmitButton className="w-full my-2" loading={onSubmit.loading}>
            Přihlásit
          </SubmitButton>

          <div className="flex justify-between flex-wrap gap-2">
            <Link href="/register" passHref>
              <a className="uppercase rounded-md px-3 text-sm py-2 text-red-500 hover:bg-red-100 text-left">
                Registrovat se
              </a>
            </Link>
            <Link href="/forgotten-password" passHref>
              <a className="uppercase rounded-md px-3 text-sm py-2 text-red-500 hover:bg-red-100 text-right">
                Zapomněli jste heslo?
              </a>
            </Link>
          </div>
        </form>
      </Card>
    </div>
  );
};

export const getServerSideProps = withServerLoggedOut;
