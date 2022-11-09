import { Button, Grid } from '@mui/material';
import { PaymentCategoryFragment, PlatbyCategoryInput, useCreatePaymentCategoryMutation, useUpdatePaymentCategoryMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { CheckboxElement, DatePickerElement, TextFieldElement } from 'react-hook-form-mui';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';

type FormProps = Pick<PlatbyCategoryInput, 'pcName' | 'pcSymbol' | 'pcAmount' | 'pcDateDue' | 'pcValidFrom' | 'pcValidTo' | 'pcUsePrefix' | 'pcArchive' | 'pcVisible'>;

export const PaymentCategoryForm: React.FC<{
  data?: PaymentCategoryFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreatePaymentCategoryMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdatePaymentCategoryMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>({
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
    <Grid container spacing={1.5} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="pcName" label="Název" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="pcSymbol" label="Specifický symbol" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="pcAmount" label="Očekávaná částka" type="number" required />
      </Grid>

      <Grid item xs={12}>
        <DatePickerElement inputProps={{ fullWidth: true }} control={control} label="Splatnost" name="pcDateDue" required />
      </Grid>
      <Grid item xs={12}>
        <DatePickerElement inputProps={{ fullWidth: true }} control={control} label="Platné od" name="pcValidFrom" required />
      </Grid>
      <Grid item xs={12}>
        <DatePickerElement inputProps={{ fullWidth: true }} control={control} label="Platné do" name="pcValidTo" required />
      </Grid>

      <Grid item xs={12}>
        <CheckboxElement control={control} name="pcUsePrefix" value="1" label="Použít prefix" />
      </Grid>
      <Grid item xs={12}>
        <CheckboxElement control={control} name="pcArchive" value="1" label="Archiv" />
      </Grid>
      <Grid item xs={12}>
        <CheckboxElement control={control} name="pcVisible" value="1" label="Viditelný" />
      </Grid>

      <Grid item xs={12}>
        <Button fullWidth variant="contained" type="submit" color="primary" disabled={onSubmit.loading}>Uložit</Button>
      </Grid>
    </Grid>
  );
};
