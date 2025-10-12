import { UpdateTenantSettingsDocument } from '@/graphql/CurrentUser';
import { TextFieldElement } from '@/ui/fields/text';
import { useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
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
  const { control, handleSubmit } = useForm<z.infer<typeof AuthForm>>({
    resolver: zodResolver(AuthForm),
  });
  const update = useMutation(UpdateTenantSettingsDocument)[1];

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof AuthForm>) => {
    await update({
      input: {
        path: ['evidenceAuth'],
        newValue: JSON.stringify(values),
      },
    });
    onSuccess();
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TextFieldElement control={control} name="login" label="Přihlašovací jméno" />
      <TextFieldElement control={control} name="password" type="password" label="Heslo" />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}
