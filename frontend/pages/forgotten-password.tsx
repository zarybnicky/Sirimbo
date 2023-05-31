import { Card } from 'components/Card';
import { ErrorBox } from 'components/ErrorBox';
import { SubmitButton } from 'components/SubmitButton';
import { useRouter } from 'next/router';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { toast } from 'react-toastify';
import type { NextPageWithLayout } from 'pages/_app';
import { ResetPasswordDocument } from 'lib/graphql/CurrentUser';
import { useMutation } from 'urql';

const Page: NextPageWithLayout = () => {
  const { control, handleSubmit } = useForm();
  const resetPassword = useMutation(ResetPasswordDocument)[1];
  const router = useRouter();

  const onSubmit = useAsyncCallback(async (data: any) => {
    await resetPassword({ input: data });
    toast.success(
      'Heslo bylo úspěšně změněno, za chvíli byste jej měli obdržet v e-mailu',
    );
    router.push('/');
  });

  return (
    <div className="container mx-auto max-w-md mt-16 mb-20">
      <Card>
        <form onSubmit={handleSubmit(onSubmit.execute)}>
          <h5 className="text-xl mb-2">Zapomenuté heslo</h5>
          <div className="mb-4">
            Pokud jste zapomněli heslo, pošleme Vám nové na e-mail, který jste zadali při
            registraci.
          </div>

          <div className="space-y-4 mb-4">
            <TextFieldElement
              control={control}
              name="login"
              label="Přihlašovací jméno"
              autoComplete="login"
              required
            />
            <TextFieldElement
              control={control}
              type="email"
              name="email"
              label="E-mail"
              autoComplete="email"
              required
            />
            <ErrorBox
              error={onSubmit.error}
              default="Nepodařilo se změnit heslo, prosím kontaktujte administrátora."
            />
          </div>
          <SubmitButton className="w-full" loading={onSubmit.loading}>
            Obnovit heslo
          </SubmitButton>
        </form>
      </Card>
    </div>
  );
};

Page.staticTitle = 'Zapomenuté heslo';
Page.requireLoggedOut = true;

export default Page;
