'use client';

import React from 'react';
import { Layout } from '@/ui/Layout';
import { SubmitButton } from '@/ui/submit';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { z } from 'zod';
import { useAsyncCallback } from 'react-async-hook';
import { registerWithoutInvitationAction } from '@/lib/server/auth-actions';
import { useRouter } from 'next/navigation';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { ErrorPage } from '@/ui/ErrorPage';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { useSetAtom } from 'jotai';
import { authAtom, sessionPresentAtom, useTenantConfig } from '@/ui/state/auth';

const Form = z.object({
  email: z.email(),
  passwd: z.string(),
});

export default function RegisterPage() {
  const { enableRegistration } = useTenantConfig();
  const router = useRouter();
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const setSessionPresent = useSetAtom(sessionPresentAtom);
  const setAuth = useSetAtom(authAtom);
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    const result = await registerWithoutInvitationAction(values);
    if (result.status === 'error') {
      throw new Error(result.error);
    }
    setAuth(result.claims, result.user);
    setSessionPresent(true);
    router.replace('/dashboard');
  });

  const personCount = auth.personIds.length;
  React.useEffect(() => {
    if (!authLoading && auth.user) {
      router.replace(personCount > 0 ? '/dashboard' : '/profil');
    }
  }, [authLoading, auth.user, personCount, router]);

  if (!enableRegistration) {
    return (
      <Layout className="grow content relative content-stretch">
        <ErrorPage
          error="Registrace je uzavřena"
          details="Nové registrace aktuálně nepřijímáme."
        />
      </Layout>
    );
  }

  return (
    <Layout className="grow content relative content-stretch">
      <div className="flex items-center justify-center h-full">
        <div className="group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1">
          <form className="grid gap-2 p-4" onSubmit={handleSubmit(onSubmit.execute)}>
            <h4 className="text-2xl">Přihláška nového člena</h4>

            <FormError error={onSubmit.error} />

            <p>
              Než začnete vyplňovat přihlášku nového člena, vytvořte si prosím uživatelský
              účet v systému.
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
              name="passwd"
              type="password"
              label="Heslo"
              autoComplete="new-password"
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
}
