import React from 'react';
import { PaymentItemFragment, useCreatePaymentItemMutation, usePaymentCategoryListQuery, useUpdatePaymentItemMutation } from 'lib/graphql/Payment';
import { useForm } from 'react-hook-form';
import { SelectElement } from 'components/SelectElement';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { PlatbyItemInput } from 'lib/graphql';
import { useUserListQuery } from 'lib/graphql/User';

type FormProps = Pick<PlatbyItemInput, 'piAmount' | 'piDate' | 'piIdCategory' | 'piIdUser' | 'piPrefix'>;

export const PaymentItemForm: React.FC<{
  data?: PaymentItemFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreatePaymentItemMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdatePaymentItemMutation({ onSuccess });

  const { data: users } = useUserListQuery();
  const { data: categories } = usePaymentCategoryListQuery();

  // load also platby_raw linked to this one
  // php-unserialize-js the blob
  // on delete, mark raw as !sorted and discarded

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: {
      piAmount: data?.piAmount,
      piDate: data?.piDate,
      piIdCategory: data?.piIdCategory,
      piIdUser: data?.piIdUser,
      piPrefix: data?.piPrefix,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.piId, patch: values });
    } else {
      await doCreate({ input: { ...values } });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} type="date" label="Datum" name="piDate" required />
      <TextFieldElement control={control} name="piAmount" label="Částka (Kč)" required />
      <SelectElement
        control={control} name="piIdUser" label="Uživatel" required
        options={(users?.users?.nodes || []).map(x => ({
          id: x.uId, label: `${x.uId.padStart(6, '0')} - ${x.uJmeno} ${x.uPrijmeni}`
        }))}
      />
      <SelectElement
        control={control} name="piIdUser" label="Uživatel" required
        options={(categories?.platbyCategories?.nodes || []).map(x => ({
          id: x.pcId, label: `${x.pcId} - ${x.pcName}`
        }))}
      />
      <TextFieldElement control={control} name="piPrefix" label="Prefix (rok)" required />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
