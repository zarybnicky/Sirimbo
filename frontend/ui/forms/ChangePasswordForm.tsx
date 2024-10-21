import React from 'react';
import { TextFieldElement } from '@/ui/fields/text';
import { useAsyncCallback } from 'react-async-hook';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { ChangePasswordDocument } from '@/graphql/CurrentUser';
import { type TypeOf, z } from 'zod';
import { useMutation } from 'urql';
import { DialogTitle } from '@/ui/dialog';
import { useZodForm } from '@/lib/use-schema-form';

const Form = z
  .object({
    newPass: z.string(),
    checkPass: z.string(),
  })
  .refine((data) => data.newPass === data.checkPass, {
    message: 'Nová hesla se neshodují',
    path: ['checkPass'],
  });

export function ChangePasswordForm() {
  const { onSuccess } = useFormResult();
  const doUpdate = useMutation(ChangePasswordDocument)[1];
  const { control, handleSubmit } = useZodForm(Form);
  const onSubmit = useAsyncCallback(async ({ newPass }: TypeOf<typeof Form>) => {
    await doUpdate({ input: { newPass } });
    onSuccess();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <DialogTitle>Změnit heslo</DialogTitle>
      <FormError error={onSubmit.error} />

      <TextFieldElement
        control={control}
        label="Nové heslo"
        name="newPass"
        type="password"
        autoComplete="new-password"
        required
      />
      <TextFieldElement
        control={control}
        label="Potvrďte nové heslo"
        name="checkPass"
        type="password"
        autoComplete="new-password"
        required
      />

      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
