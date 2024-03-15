import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from '@/ui/fields/text';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { ChangePasswordDocument } from '@/graphql/CurrentUser';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { useMutation } from 'urql';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@/ui/dialog';
import { buttonCls } from '@/ui/style';

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

export function ChangePasswordDialog({ onSuccess }: { onSuccess?: () => void; }) {
  const [passOpen, setPassOpen] = React.useState(false);

  const doUpdate = useMutation(ChangePasswordDocument)[1];
  const { control, handleSubmit } = useForm<FormProps>({ resolver: zodResolver(Form) });
  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpdate({ input: { oldPass: values.oldPass, newPass: values.newPass } });
    setPassOpen(false);
    onSuccess?.();
  });

  return (
    <Dialog open={passOpen} onOpenChange={setPassOpen}>
      <DialogTrigger asChild>
        <button className={buttonCls({ size: 'sm', variant: 'outline' })}>Změnit heslo</button>
      </DialogTrigger>
      <DialogContent>
        <DialogTitle>Změnit heslo</DialogTitle>
        <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
          <FormError error={onSubmit.error} />

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
      </DialogContent>
    </Dialog>
  );
};
