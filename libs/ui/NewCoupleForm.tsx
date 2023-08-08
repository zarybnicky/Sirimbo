import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { ComboboxElement } from '@app/ui/Combobox';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { UserListDocument } from '@app/graphql/User';
import { CreateCoupleDocument } from '@app/graphql/Couple';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  man: z.string(),
  woman: z.string(),
});
type FormProps = z.infer<typeof Form>;

export const NewCoupleForm: React.FC<{
  onSuccess?: () => void;
}> = ({ onSuccess }) => {
  const [{ data: users }] = useQuery({ query: UserListDocument });
  const men = React.useMemo(
    () =>
      (users?.users?.nodes || [])
        .filter((x) => x.uPohlavi === 'm')
        .map((x) => ({ id: x.id, label: `${x.uJmeno} ${x.uPrijmeni} (${x.id})` })),
    [users],
  );
  const women = React.useMemo(
    () =>
      (users?.users?.nodes || [])
        .filter((x) => x.uPohlavi === 'f')
        .map((x) => ({ id: x.id, label: `${x.uJmeno} ${x.uPrijmeni} (${x.id})` })),
    [users],
  );

  const doCreate = useMutation(CreateCoupleDocument)[1];

  const { control, handleSubmit } = useForm<FormProps>({ resolver: zodResolver(Form) });
  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doCreate({
      input: {
        couple: {
          manId: values.man,
          womanId: values.woman,
          active: true,
          since: new Date().toString(),
        },
      },
    });
    onSuccess?.();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />
      <ComboboxElement
        control={control}
        name="man"
        label="Partner"
        placeholder="vyberte partnera"
        options={men}
      />
      <ComboboxElement
        control={control}
        name="woman"
        label="Partnerka"
        placeholder="vyberte partnerku"
        options={women}
      />
      <SubmitButton loading={onSubmit.loading}>Spárovat</SubmitButton>
    </form>
  );
};
