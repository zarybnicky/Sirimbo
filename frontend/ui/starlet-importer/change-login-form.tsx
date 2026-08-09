import { UpdateTenantSettingsDocument } from '@/graphql/CurrentUser';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useMutation } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const AuthForm = z.object({
  login: z.string(),
  password: z.string(),
});

export function ChangeLoginForm() {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit } = useForm({
    mode: 'onBlur',
    resolver: zodResolver(AuthForm),
  });
  const [result, update] = useMutation(UpdateTenantSettingsDocument);

  const onSubmit = async (values: z.infer<typeof AuthForm>) => {
    const result = await update({
      input: {
        path: ['evidenceAuth'],
        newValue: JSON.stringify(values),
      },
    });
    if (!result.error) onSuccess();
  };

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error} />
      <TextFieldElement control={control} name="login" label="Přihlašovací jméno" />
      <TextFieldElement control={control} name="password" type="password" label="Heslo" />
      <SubmitButton control={control} />
    </form>
  );
}
