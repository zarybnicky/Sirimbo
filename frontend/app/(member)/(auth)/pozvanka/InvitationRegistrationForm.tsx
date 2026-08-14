'use client';

import { useAuth } from '@/lib/auth';
import { registerUsingInvitationAction } from '@/lib/auth-actions';
import { TextField, TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import { redirect } from 'next/navigation';
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
  const auth = useAuth();
  if (auth.user) redirect(auth.personIds.length > 0 ? '/dashboard' : '/profil');

  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues: { token: token ?? '', email: email ?? '', passwd: '' },
  });
  const [error, setError] = React.useState('');
  const onSubmit = async (values: z.infer<typeof Form>) => {
    setError('');
    const message = await registerUsingInvitationAction(values);
    if (message) setError(message);
  };

  return (
    <div className="group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1">
      <form className="grid gap-2 p-4" onSubmit={handleSubmit(onSubmit)}>
        <h4 className="text-2xl">Registrace nového uživatele</h4>

        <FormError error={error} />

        <p>
          Přišla vám pozvánka do klubového systému. Nastavte si heslo a vytvořte si účet.
        </p>

        {name && <TextField name="name" label="Osoba" value={name} readOnly />}

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
          disabled={!email}
        />
        <SubmitButton control={control} className="w-full my-2" disabled={!email}>
          Registrovat
        </SubmitButton>
      </form>
    </div>
  );
}
