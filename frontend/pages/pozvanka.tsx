import React from 'react';
import { Layout } from '@/ui/Layout';
import { parseAsString, useQueryState } from 'nuqs';
import { SubmitButton } from '@/ui/submit';
import { TextField, TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { z } from 'zod';
import { useMutation, useQuery } from 'urql';
import {
  InvitationInfoDocument,
  RegisterUsingInvitationDocument,
} from '@/graphql/CurrentUser';
import { useRouter } from 'next/router';
import { NextSeo } from 'next-seo';
import Link from 'next/link';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const InvitationToken = z.uuid();

const Form = z.object({
  email: z.email(),
  passwd: z.string().min(1, 'Zadejte heslo'),
  token: InvitationToken,
});

export default function InvitationPage() {
  const router = useRouter();
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const [token] = useQueryState('token', parseAsString.withDefault(''));
  const { setValue, control, handleSubmit } = useForm({
    mode: 'onBlur',
    resolver: zodResolver(Form),
  });

  const isValidToken = InvitationToken.safeParse(token).success;
  const [{ data, fetching }] = useQuery({
    query: InvitationInfoDocument,
    variables: { token },
    pause: !isValidToken || !router.isReady,
  });
  const [result, register] = useMutation(RegisterUsingInvitationDocument);

  React.useEffect(() => {
    setValue('token', token);
    setValue('email', data?.invitationInfo || '');
  }, [data, setValue, token]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const response = await register({ input: values });
    if (!response.error && response.data?.registerUsingInvitation?.result?.jwt) {
      await router.replace('/dashboard');
    }
  };

  if (!authLoading && auth.user) {
    void router.replace(auth.personIds.length === 0 ? '/profil' : '/dashboard');
  }

  return (
    <Layout className="grow content relative content-stretch">
      <NextSeo title="Registrace" />

      <div className="flex items-center justify-center h-full">
        <div className="group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1">
          <form className="grid gap-2 p-4" onSubmit={handleSubmit(onSubmit)}>
            <h4 className="text-2xl">Registrace nového uživatele</h4>

            <FormError error={result.error} />
            {router.isReady && !isValidToken && (
              <FormError
                default="Vaše pozvánka není platná."
                error={
                  <>
                    Pokud jste se již registrovali,{' '}
                    <Link href="/dashboard">přihlaste se zde</Link>.
                  </>
                }
              />
            )}

            {isValidToken && !fetching && !data?.invitationInfo && (
              <FormError
                default="Vaše pozvánka je neplatná nebo již použitá."
                error={
                  <>
                    Pokud jste se již registrovali,{' '}
                    <Link href="/dashboard">přihlaste se zde</Link>.
                  </>
                }
              />
            )}

            <p>
              Někdo ti poslal pozvánku do klubového systému. Nastav si heslo a vytvoř si
              účet.
            </p>

            {data?.invitationName && (
              <TextField name="name" label="Osoba" value={data.invitationName} readOnly />
            )}

            <TextFieldElement
              control={control}
              name="email"
              label="E-mail"
              autoComplete="email"
              readOnly
            />

            <TextFieldElement
              control={control}
              name="passwd"
              type="password"
              label="Heslo"
              autoComplete="new-password"
              required
            />
            <SubmitButton control={control} className="w-full my-2">
              Registrovat
            </SubmitButton>
          </form>
        </div>
      </div>
    </Layout>
  );
}
