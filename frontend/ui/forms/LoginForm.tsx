'use client';

import { loginAction } from '@/lib/auth-actions';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { useTenantConfig } from '@/ui/state/auth';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import Link from 'next/link';
import { useSearchParams } from 'next/navigation';
import { useForm } from 'react-hook-form';
import { z } from 'zod';
import { useRedirectLoggedIn } from '../use-auth';
import React from 'react';

const Form = z.object({
  login: z.string().min(1, 'Zadejte přihlašovací jméno nebo e-mail'),
  passwd: z.string().min(1, 'Zadejte heslo'),
});

export function LoginForm() {
  const { enableRegistration } = useTenantConfig();
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  useRedirectLoggedIn();

  const [error, setError] = React.useState('');

  const search = useSearchParams();
  const from = search?.get('from');

  const onSubmit = async (values: z.infer<typeof Form>) => {
    setError('');
    const message = await loginAction(values, from);
    if (message) setError(message);
  };

  return (
    <form className="grid gap-2 p-4" onSubmit={handleSubmit(onSubmit)}>
      <h2 className="text-xl">Přihlášení do systému</h2>

      <FormError error={error} />
      <TextFieldElement
        control={control}
        name="login"
        label="E-mail nebo přihlašovací jméno"
        autoComplete="username"
        required
        autoFocus
      />
      <TextFieldElement
        control={control}
        name="passwd"
        type="password"
        label="Heslo"
        autoComplete="current-password"
        required
      />
      <SubmitButton control={control} className="my-2">
        Přihlásit
      </SubmitButton>

      <div className="flex flex-wrap mt-2 -mx-3 gap-2 justify-between">
        <div>
          {enableRegistration && (
            <Link
              href="/registrace"
              className="uppercase rounded-md px-3 text-sm py-2 text-accent-10 hover:bg-accent-3 text-left"
            >
              Registrace nového člena
            </Link>
          )}
        </div>
        <div>
          <Link
            href="/zapomenute-heslo"
            className="uppercase rounded-md px-3 text-sm py-2 text-accent-10 hover:bg-accent-3 text-right"
          >
            Zapomněli jste heslo?
          </Link>
        </div>
      </div>
    </form>
  );
}
