import { PaymentCategoryFragment, PlatbyCategoryInput, useCreatePaymentCategoryMutation, useUpdatePaymentCategoryMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { DatePickerElement } from 'react-hook-form-mui';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';

type FormProps = Pick<PlatbyCategoryInput, 'pcName' | 'pcSymbol' | 'pcAmount' | 'pcDateDue' | 'pcValidFrom' | 'pcValidTo' | 'pcUsePrefix' | 'pcArchive' | 'pcVisible'>;

export const PaymentCategoryForm: React.FC<{
  data?: PaymentCategoryFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreatePaymentCategoryMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdatePaymentCategoryMutation({ onSuccess });

  const { control, handleSubmit, formState } = useForm<FormProps>({
    defaultValues: {
      pcName: data?.pcName,
      pcSymbol: data?.pcSymbol,
      pcAmount: data?.pcAmount,
      pcDateDue: data?.pcDateDue,
      pcValidFrom: data?.pcValidFrom,
      pcValidTo: data?.pcValidTo,
      pcUsePrefix: data?.pcUsePrefix,
      pcArchive: data?.pcArchive,
      pcVisible: data?.pcVisible,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.pcId, patch: values });
    } else {
      await doCreate({ input: { ...values, pcUseBase: false } });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="pcName" label="Název" required />
      <TextFieldElement control={control} name="pcSymbol" label="Specifický symbol" required />
      <TextFieldElement control={control} name="pcAmount" label="Očekávaná částka" type="number" required />
      <DatePickerElement inputProps={{ fullWidth: true }} control={control} label="Splatnost" name="pcDateDue" required />
      <DatePickerElement inputProps={{ fullWidth: true }} control={control} label="Platné od" name="pcValidFrom" required />
      <DatePickerElement inputProps={{ fullWidth: true }} control={control} label="Platné do" name="pcValidTo" required />
      <CheckboxElement control={control} name="pcUsePrefix" value="1" label="Použít prefix" />
      <CheckboxElement control={control} name="pcArchive" value="1" label="Archiv" />
      <CheckboxElement control={control} name="pcVisible" value="1" label="Viditelný" />
      <SubmitButton loading={onSubmit.loading} disabled={!formState.isValid} />
    </form>
  );
};
