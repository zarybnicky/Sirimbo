import {
  PaymentCategoryFragment,
  useCreatePaymentCategoryMutation,
  useUpdatePaymentCategoryMutation,
} from 'lib/graphql/Payment';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { PlatbyCategoryInput } from 'lib/graphql';
import { pipe } from 'fp-ts/lib/function';
import { pick } from 'lib/form-utils';

const fields = [
  'pcName',
  'pcSymbol',
  'pcAmount',
  'pcDateDue',
  'pcValidFrom',
  'pcValidTo',
  'pcUsePrefix',
  'pcArchive',
  'pcVisible',
] as const;
type FormProps = Pick<PlatbyCategoryInput, (typeof fields)[number]>;

export const PaymentCategoryForm: React.FC<{
  data?: PaymentCategoryFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreatePaymentCategoryMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdatePaymentCategoryMutation({ onSuccess });

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    if (data) {
      reset(pipe(data, pick(fields)));
    }
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (patch: FormProps) => {
    if (data) {
      await doUpdate({ id: data.id, patch });
    } else {
      await doCreate({ input: patch });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="pcName" label="Název" required />
      <TextFieldElement
        control={control}
        name="pcSymbol"
        label="Specifický symbol"
        required
      />
      <TextFieldElement
        control={control}
        name="pcAmount"
        label="Očekávaná částka"
        type="number"
        required
      />
      <TextFieldElement
        control={control}
        type="date"
        label="Splatnost"
        name="pcDateDue"
        required
      />
      <TextFieldElement
        control={control}
        type="date"
        label="Platné od"
        name="pcValidFrom"
        required
      />
      <TextFieldElement
        control={control}
        type="date"
        label="Platné do"
        name="pcValidTo"
        required
      />
      <CheckboxElement
        control={control}
        name="pcUsePrefix"
        value="1"
        label="Použít prefix"
      />
      <CheckboxElement control={control} name="pcArchive" value="1" label="Archiv" />
      <CheckboxElement control={control} name="pcVisible" value="1" label="Viditelný" />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
