'use client';

import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { redirect, useRouter } from 'next/navigation';
import { TextFieldElement } from '@/ui/fields/text';
import { toast } from 'react-toastify';
import { ResetPasswordDocument } from '@/graphql/CurrentUser';
import { useMutation } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { useAuth } from '@/lib/auth';

const Form = z.object({
  email: z.email(),
});

export function ForgottenPasswordForm() {
  const auth = useAuth();
  if (auth.user) redirect(auth.personIds.length > 0 ? '/dashboard' : '/profil');

  const router = useRouter();
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  const [result, resetPassword] = useMutation(ResetPasswordDocument);

  const onSubmit = async (data: z.infer<typeof Form>) => {
    const result = await resetPassword({ input: data });
    if (!result.error && result.data?.resetPassword?.__typename) {
      toast.success(
        'Pokud byl e-mail správný, tak za chvíli najdete e-mail s pokyny ve své schránce.',
      );
      router.push('/login');
    }
  };

  return (
    <form onSubmit={handleSubmit(onSubmit)}>
      <h5 className="text-xl mb-2">Zapomenuté heslo</h5>
      <div className="mb-4">
        Pokud jste zapomněli heslo, pošleme Vám na e-mail odkaz, kde si ho můžete změnit.
      </div>

      <div className="space-y-4 mb-4">
        <TextFieldElement
          control={control}
          type="email"
          name="email"
          label="E-mail"
          autoComplete="email"
          required
        />
        <FormError error={result.error} default="Nepodařilo se obnovit heslo." />
      </div>
      <SubmitButton control={control} className="w-full">
        Obnovit heslo
      </SubmitButton>
    </form>
  );
}
