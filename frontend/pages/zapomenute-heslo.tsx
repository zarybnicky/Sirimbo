import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useRouter } from 'next/router';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { TextFieldElement } from '@/ui/fields/text';
import { toast } from 'react-toastify';
import { ResetPasswordDocument } from '@/graphql/CurrentUser';
import { useMutation } from 'urql';
import { NextSeo } from 'next-seo';
import { type TypeOf, z } from 'zod';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { Layout } from '@/components/layout/Layout';
import { useZodForm } from '@/lib/use-schema-form';
import { cardCls } from '@/ui/style';

const Form = z.object({
  email: z.string().email(),
});

function ForgottenPasswordForm() {
  const router = useRouter();
  const { control, handleSubmit } = useZodForm(Form);
  const resetPassword = useMutation(ResetPasswordDocument)[1];

  const onSubmit = useAsyncCallback(async (data: TypeOf<typeof Form>) => {
    const res = await resetPassword({ input: data });
    if (res.data?.resetPassword?.__typename) {
      toast.success(
        'Pokud byl e-mail správný, tak za chvíli najdete e-mail s pokyny ve své schránce.',
      );
      await router.push('/login');
    }
  });

  return (
    <form onSubmit={handleSubmit(onSubmit.execute)}>
      <h5 className="text-xl mb-2">Zapomenuté heslo</h5>
      <div className="mb-4">
        Pokud jste zapomněli heslo, pošleme Vám na e-mail odkaz, kde si ho můžete změnit.
      </div>

      <div className="space-y-4 mb-4">
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
          default="Nepodařilo se obnovit heslo."
        />
      </div>
      <SubmitButton className="w-full" loading={onSubmit.loading}>
        Obnovit heslo
      </SubmitButton>
    </form>
);
}

export default function ForgottenPasswordPage() {
  const router = useRouter();
  const auth = useAuth();
  const authLoading = useAuthLoading();

  if (!authLoading && auth.user) {
    void router.replace('/dashboard');
  }
  return (
    <Layout className="grow content relative content-stretch">
      <NextSeo title="Zapomenuté heslo" />
      <div className="flex h-[calc(100vh-80px)] items-center justify-center p-5 bg-neutral-1 w-full">
        <div className={cardCls({ className: "p-4 max-w-lg" })}>
          <ForgottenPasswordForm />
        </div>
      </div>
    </Layout>
  );
};
