'use client';

import { acceptInvitationAction } from '@/lib/auth-actions';
import { TextField, TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { cardCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import Link from 'next/link';
import React from 'react';
import { useForm } from 'react-hook-form';
import { z } from 'zod';

const Form = z.object({
  email: z.email(),
  passwd: z.string().min(1, 'Zadejte heslo'),
  token: z.uuid(),
});

type Props = {
  token: string;
  email: string;
  name: string;
};

export function InvitationRegistrationForm({ token, email, name }: Props) {
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues: { token: token ?? '', email: email ?? '', passwd: '' },
  });
  const [error, setError] = React.useState('');
  const onSubmit = async (values: z.infer<typeof Form>) => {
    setError('');
    const result = await acceptInvitationAction(values);
    if ('error' in result) {
      setError(result.error);
    } else {
      window.location.assign(result.redirectTo);
    }
  };

  return (
    <div className={cardCls()}>
      <form className="grid gap-2 p-4" onSubmit={handleSubmit(onSubmit)}>
        <h4 className="text-2xl">Registrace nového uživatele</h4>

        <FormError error={error} />

        <p>Přišla vám pozvánka do klubového systému. Vyberte e-mail a nastavte si heslo.</p>

        {name && <TextField name="name" label="Osoba" value={name} readOnly />}

        <TextFieldElement
          control={control}
          name="email"
          label="E-mail"
          autoComplete="email"
          required
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
        <p className="text-sm text-neutral-11">
          Už účet máte?{' '}
          <Link
            className="text-accent-11 underline"
            href={`/login?from=${encodeURIComponent(`/pozvanka?token=${token}`)}`}
          >
            Přihlaste se
          </Link>{' '}
          a přijměte pozvánku do něj.
        </p>
      </form>
    </div>
  );
}
