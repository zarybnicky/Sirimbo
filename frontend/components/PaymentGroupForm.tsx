import { PaymentGroupFragment, PlatbyGroupInput, useCreatePaymentGroupMutation, useUpdatePaymentGroupMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextAreaElement, TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';

type FormProps = Pick<PlatbyGroupInput, 'pgName' | 'pgDescription' | 'pgBase'>;

export const PaymentGroupForm: React.FC<{
  data?: PaymentGroupFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreatePaymentGroupMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdatePaymentGroupMutation({ onSuccess });

  const { control, handleSubmit, formState } = useForm<FormProps>({
    defaultValues: {
      pgName: data?.pgName,
      pgDescription: data?.pgDescription,
      pgBase: data?.pgBase,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.pgId, patch: values });
    } else {
      await doCreate({ input: { ...values, pgType: '1' } });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="pgName" label="Název" required />
      <TextAreaElement control={control} name="pgDescription" label="Shrnutí" rows={3} required />
      <TextFieldElement control={control} type="number" name="pgBase" label="Násobitel částky" required />
      <SubmitButton loading={onSubmit.loading} disabled={!formState.isValid} />
    </form>
  );
};
