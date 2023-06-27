import { Card } from '@app/ui/Card';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { useRouter } from 'next/router';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from '@app/ui/fields/text';
import { toast } from 'react-toastify';
import type { NextPageWithLayout } from 'pages/_app';
import { ResetPasswordDocument } from '@app/graphql/CurrentUser';
import { useMutation } from 'urql';
import { NextSeo } from 'next-seo';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  login: z.string(),
  email: z.string().email(),
});
type FormProps = z.infer<typeof Form>

const Page: NextPageWithLayout = () => {
  const { control, handleSubmit } = useForm<FormProps>({ resolver: zodResolver(Form) });
  const resetPassword = useMutation(ResetPasswordDocument)[1];
  const router = useRouter();

  const onSubmit = useAsyncCallback(async (data: FormProps) => {
    await resetPassword({ input: data });
    toast.success(
      'Heslo bylo úspěšně změněno, za chvíli byste jej měli obdržet v e-mailu',
    );
    await router.push('/');
  });

  return (
    <div className="container mx-auto max-w-md mt-16 mb-20">
      <NextSeo title="Zapomenuté heslo" />
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
            <FormError
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

Page.requireLoggedOut = true;

export default Page;
