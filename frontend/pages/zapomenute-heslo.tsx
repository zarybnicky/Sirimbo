import { Card } from '@app/ui/Card';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { useRouter } from 'next/router';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { TextFieldElement } from '@app/ui/fields/text';
import { toast } from 'react-toastify';
import { ResetPasswordDocument } from '@app/graphql/CurrentUser';
import { useMutation } from 'urql';
import { NextSeo } from 'next-seo';
import { TypeOf, z } from 'zod';
import { useAuth } from '@app/ui/use-auth';
import { Layout } from '@/components/layout/Layout';
import { useZodForm } from '@/lib/use-schema-form';

const Form = z.object({
  login: z.string(),
  email: z.string().email(),
});

const Page = () => {
  const router = useRouter();
  const { user, isLoading } = useAuth();
  const { control, handleSubmit } = useZodForm(Form);
  const resetPassword = useMutation(ResetPasswordDocument)[1];

  const onSubmit = useAsyncCallback(async (data: TypeOf<typeof Form>) => {
    await resetPassword({ input: data });
    toast.success(
      'Heslo bylo úspěšně změněno, za chvíli byste jej měli obdržet v e-mailu',
    );
    await router.push('/');
  });

  if (!isLoading && user) {
    void router.replace('/dashboard');
  }
  return (
    <Layout>
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
    </Layout>
  );
};

export default Page;
