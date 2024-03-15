import React from 'react';
import { Layout } from '@/components/layout/Layout';
import { SubmitButton } from '@/ui/submit';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { useZodForm } from '@/lib/use-schema-form';
import { TypeOf, z } from 'zod';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation } from 'urql';
import { RegisterWithoutInvitationDocument } from '@/graphql/CurrentUser';
import { useRouter } from 'next/router';
import { NextSeo } from 'next-seo';

const Form = z.object({
  login: z.string(),
  email: z.string().email(),
  passwd: z.string(),
});

function InvitationPage() {
  const router = useRouter();
  const { control, handleSubmit } = useZodForm(Form);

  const register = useMutation(RegisterWithoutInvitationDocument)[1];

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    const response = await register({ input: values });
    console.log(response);
    if (response.data?.registerWithoutInvitation?.result?.jwt) {
      router.replace('/profil');
    }
  });

  return (
    <Layout className="grow content relative content-stretch">
      <NextSeo title="Přihláška nového člena" />

    <div className="flex items-center justify-center h-full">
      <div className="group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1">
        <form className="grid gap-2 p-4" onSubmit={handleSubmit(onSubmit.execute)}>
          <h4 className="text-2xl">Přihláška nového člena</h4>

          <FormError error={onSubmit.error} />

          <p>
            Než začnete vyplňovat přihlášku nového člena, vytvořte si prosím uživatelský účet v systému.
          </p>

          <TextFieldElement
            control={control}
            name="email"
            label="E-mail"
            autoComplete="email"
            required
            autoFocus
          />

          <TextFieldElement
            control={control}
            name="login"
            label="Přihlašovací jméno"
            autoComplete="username"
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
            Vytvořit účet
          </SubmitButton>
        </form>
      </div>
    </div>
    </Layout>
  );
};

export default InvitationPage;
