import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { ChangePasswordDocument } from '@/graphql/CurrentUser';
import { z } from 'zod';
import { useMutation } from 'urql';
import { DialogTitle } from '@/ui/dialog';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z
  .object({
    newPass: z.string().min(1, 'Zadejte nové heslo'),
    checkPass: z.string().min(1, 'Potvrďte nové heslo'),
  })
  .refine((data) => data.newPass === data.checkPass, {
    path: ['checkPass'],
    error: 'Nová hesla se neshodují',
  });

export function ChangePasswordForm() {
  const { onSuccess } = useFormResult();
  const [result, doUpdate] = useMutation(ChangePasswordDocument);
  const { control, handleSubmit } = useForm({
    mode: 'onBlur',
    resolver: zodResolver(Form),
  });
  const onSubmit = async ({ newPass }: z.infer<typeof Form>) => {
    const result = await doUpdate({ input: { newPass } });
    if (!result.error) onSuccess();
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <DialogTitle>Změnit heslo</DialogTitle>
      <FormError error={result.error} />

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

      <SubmitButton control={control} />
    </form>
  );
}
