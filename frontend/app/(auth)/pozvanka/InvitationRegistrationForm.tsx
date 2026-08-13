'use client';

import type { RegisterUsingInvitationInput } from '@/graphql/CurrentUser';
import { TextField, TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useRedirectLoggedIn } from '@/ui/use-auth';
import { zodResolver } from '@hookform/resolvers/zod';
import Link from 'next/link';
import { useRouter } from 'next/navigation';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { z } from 'zod';

const Form = z.object({
  email: z.email(),
  passwd: z.string().min(1, 'Zadejte heslo'),
  token: z.uuid(),
});

export function InvitationRegistrationForm({
  register,
  token,
  email,
  name,
}: {
  register: (input: RegisterUsingInvitationInput) => Promise<string | undefined>;
  token?: string;
  email?: string;
  name?: string;
}) {
  const router = useRouter();
  useRedirectLoggedIn();
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues: { token: token ?? '', email: email ?? '', passwd: '' },
  });
  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    const error = await register(values);
    if (error) throw new Error(error);
    router.replace('/dashboard');
  });

  return (
    <div className="flex items-center justify-center h-full">
      <div className="group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1">
        <form className="grid gap-2 p-4" onSubmit={handleSubmit(onSubmit.execute)}>
          <h4 className="text-2xl">Registrace nového uživatele</h4>

          <FormError error={onSubmit.error} />
          {!email && (
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
    </div>
  );
}
