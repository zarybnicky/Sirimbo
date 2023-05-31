import {
    CreatePaymentGroupDocument,
  PaymentGroupFragment,
  UpdatePaymentGroupDocument,
} from 'lib/graphql/Payment';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextAreaElement, TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { PlatbyGroupInput } from 'lib/graphql';
import { useMutation } from 'urql';

type FormProps = Pick<PlatbyGroupInput, 'pgName' | 'pgDescription' | 'pgBase'>;

export const PaymentGroupForm: React.FC<{
  data?: PaymentGroupFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const doCreate = useMutation(CreatePaymentGroupDocument)[1];
  const doUpdate = useMutation(UpdatePaymentGroupDocument)[1];

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      pgName: data?.pgName,
      pgDescription: data?.pgDescription,
      pgBase: data?.pgBase,
    });
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.id, patch: values });
    } else {
      await doCreate({ input: { ...values, pgType: '1' } });
    }
    onSuccess()
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <SubmitButton loading={onSubmit.loading} />

      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="pgName" label="Název" required />
      <TextAreaElement
        control={control}
        name="pgDescription"
        label="Shrnutí"
        rows={3}
        required
      />
      <TextFieldElement
        control={control}
        type="number"
        name="pgBase"
        label="Násobitel částky"
        required
      />
    </form>
  );
};
