'use client';

import type { RegisterWithoutInvitationInput } from '@/graphql/CurrentUser';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useRedirectLoggedIn } from '@/ui/use-auth';
import { zodResolver } from '@hookform/resolvers/zod';
import { useRouter } from 'next/navigation';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { z } from 'zod';

const Form = z.object({
  email: z.email(),
  passwd: z.string().min(1, 'Zadejte heslo'),
});

export function RegistrationForm({
  register,
}: {
  register: (input: RegisterWithoutInvitationInput) => Promise<string | undefined>;
}) {
  const router = useRouter();
  useRedirectLoggedIn();
  const { control, handleSubmit } = useForm({ resolver: zodResolver(Form) });
  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    const error = await register(values);
    if (error) throw new Error(error);
    router.replace('/dashboard');
  });

  return (
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

          <SubmitButton control={control} className="w-full my-2">
            Vytvořit účet
          </SubmitButton>
        </form>
      </div>
    </div>
  );
}
