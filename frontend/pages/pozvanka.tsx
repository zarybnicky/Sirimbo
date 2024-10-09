import React from 'react';
import { Layout } from '@/components/layout/Layout';
import { StringParam, useQueryParam, withDefault } from 'use-query-params';
import { SubmitButton } from '@/ui/submit';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { useZodForm } from '@/lib/use-schema-form';
import { type TypeOf, z } from 'zod';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { InvitationInfoDocument, RegisterUsingInvitationDocument } from '@/graphql/CurrentUser';
import { useRouter } from 'next/router';
import { NextSeo } from 'next-seo';
import Link from 'next/link';
import { useAuth, useAuthLoading } from '@/ui/use-auth';

const Form = z.object({
    login: z.string(),
    email: z.string().email(),
    passwd: z.string(),
    token: z.string(),
});

function InvitationPage() {
  const router = useRouter();
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const [token] = useQueryParam('token', withDefault(StringParam, ''));
  const { setValue, control, handleSubmit } = useZodForm(Form);

  const [{ data, fetching }] = useQuery({ query: InvitationInfoDocument, variables: { token }, pause: !token });
  const register = useMutation(RegisterUsingInvitationDocument)[1];

  React.useEffect(() => {
    setValue('token', token);
    setValue('email', data?.invitationInfo || '');
  }, [data, setValue, token]);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    const response = await register({ input: values });
    if (response.data?.registerUsingInvitation?.result?.jwt) {
      router.replace('/dashboard');
    }
  });

  if (!authLoading && auth.user) {
    void router.replace(!auth.personIds.length ? '/profil' :'/dashboard');
  }

  return (
    <Layout className="grow content relative content-stretch">
      <NextSeo title="Registrace" />

    <div className="flex items-center justify-center h-full">
      <div className="group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1">
        <form className="grid gap-2 p-4" onSubmit={handleSubmit(onSubmit.execute)}>
          <h4 className="text-2xl">Registrace nového uživatele</h4>

          <FormError error={onSubmit.error} />
          {!fetching && !data?.invitationInfo && (
            <FormError
              error={
                <>
                  Vaše pozvánka je neplatná nebo již použitá. Pokud jste se již registrovali,
                  {' '}
                  <Link href="/dashboard">přihlašte se zde</Link>.
                </>
              }
            />
          )}

          <p>
            Někdo ti poslal pozvánku do klubového systému. Nastav si heslo, vyplň své přihlašovací jméno a případně si změň heslo, které bude uložené v systému.
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
            Přihlásit
          </SubmitButton>
        </form>
      </div>
    </div>
    </Layout>
  );
};

export default InvitationPage;
