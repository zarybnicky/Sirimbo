import { Button, Grid } from '@mui/material';
import { PaymentGroupFragment, PlatbyGroupInput, useCreatePaymentGroupMutation, useUpdatePaymentGroupMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'react-hook-form-mui';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';

type FormProps = Pick<PlatbyGroupInput, 'pgName' | 'pgDescription' | 'pgBase'>;

export const PaymentGroupForm: React.FC<{
  data?: PaymentGroupFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreatePaymentGroupMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdatePaymentGroupMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>({
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
    <Grid container spacing={1.5} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="pgName" label="Název" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="pgDescription" label="Shrnutí" rows={3} multiline required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} type="number" name="pgBase" label="Násobitel částky" required />
      </Grid>
      <Grid item xs={12}>
        <Button fullWidth variant="contained" type="submit" color="primary" disabled={onSubmit.loading}>Uložit</Button>
      </Grid>
    </Grid>
  );
};
