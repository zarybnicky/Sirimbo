import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from 'components/ErrorBox';
import { SubmitButton } from './SubmitButton';
import { ChangePasswordDocument } from '@app/graphql/CurrentUser';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { useMutation } from 'urql';

const Form = z
  .object({
    oldPass: z.string(),
    newPass: z.string(),
    checkPass: z.string(),
  })
  .refine((data) => data.newPass === data.checkPass, {
    message: 'Nová hesla se neshodují',
    path: ['checkPass'],
  });
type FormProps = z.infer<typeof Form>;

export const ChangePasswordForm: React.FC<{
  onSuccess: () => void;
}> = ({ onSuccess }) => {
  const doUpdate = useMutation(ChangePasswordDocument)[1];
  const { control, handleSubmit } = useForm<FormProps>({ resolver: zodResolver(Form) });
  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpdate({ input: { oldPass: values.oldPass, newPass: values.newPass } });
    onSuccess?.();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />

      <TextFieldElement
        control={control}
        label="Staré heslo"
        name="oldPass"
        type="password"
        autoComplete="current-password"
        required
      />
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
