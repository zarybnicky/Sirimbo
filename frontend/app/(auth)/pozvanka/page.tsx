'use client';

import React from 'react';
import { Layout } from '@/ui/Layout';
import { parseAsString, useQueryState } from 'nuqs';
import { SubmitButton } from '@/ui/submit';
import { TextField, TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { z } from 'zod';
import { useAsyncCallback } from 'react-async-hook';
import { useQuery } from 'urql';
import { InvitationInfoDocument } from '@/graphql/CurrentUser';
import { registerUsingInvitationAction } from '@/lib/server/auth-actions';
import { useRouter } from 'next/navigation';
import Link from 'next/link';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { useSetAtom } from 'jotai';
import { sessionPresentAtom } from '@/ui/state/auth';

const InvitationToken = z.uuid();

const Form = z.object({
  email: z.email(),
  passwd: z.string(),
  token: InvitationToken,
});

export default function InvitationPage() {
  const router = useRouter();
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const setSessionPresent = useSetAtom(sessionPresentAtom);
  const [token] = useQueryState('token', parseAsString.withDefault(''));
  const { setValue, control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });

  const isValidToken = InvitationToken.safeParse(token).success;
  const [{ data, fetching }] = useQuery({
    query: InvitationInfoDocument,
    variables: { token },
    pause: !isValidToken,
  });

  React.useEffect(() => {
    setValue('token', token);
    setValue('email', data?.invitationInfo || '');
  }, [data, setValue, token]);

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    const result = await registerUsingInvitationAction(values);
    if (result.status === 'error') {
      throw new Error(result.error);
    }
    setSessionPresent(true);
    router.replace('/dashboard');
  });

  React.useEffect(() => {
    if (!authLoading && auth.user) {
      router.replace(auth.personIds.length === 0 ? '/profil' : '/dashboard');
    }
  }, [authLoading, auth.user, auth.personIds.length, router]);

  return (
    <Layout className="grow content relative content-stretch">
      <div className="flex items-center justify-center h-full">
        <div className="group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1">
          <form className="grid gap-2 p-4" onSubmit={handleSubmit(onSubmit.execute)}>
            <h4 className="text-2xl">Registrace nového uživatele</h4>

            <FormError error={onSubmit.error} />
            {!isValidToken && (
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
            <SubmitButton className="w-full my-2" loading={onSubmit.loading}>
              Registrovat
            </SubmitButton>
          </form>
        </div>
      </div>
    </Layout>
  );
}
