import { Button, Grid } from '@mui/material';
import { PaymentItemFragment, PlatbyItemInput, useCreatePaymentItemMutation, useUpdatePaymentItemMutation, useUserListQuery } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { DatePickerElement, SelectElement, TextFieldElement } from 'react-hook-form-mui';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';

type FormProps = Pick<PlatbyItemInput, 'piAmount' | 'piDate' | 'piIdCategory' | 'piIdUser' | 'piPrefix'>;

export const PaymentItemForm: React.FC<{
  data?: PaymentItemFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreatePaymentItemMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdatePaymentItemMutation({ onSuccess });

  const { data: users } = useUserListQuery();
  // load categories (VS)
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
    <Grid container spacing={1.5} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <DatePickerElement inputProps={{ fullWidth: true }} control={control} label="Datum" name="piDate" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="piAmount" label="Částka (Kč)" required />
      </Grid>
      <Grid item xs={12}>
        <SelectElement
          fullWidth control={control} name="piIdUser" label="Uživatel" required
          options={(users?.users?.nodes || []).map(x => ({
            id: x.uId, label: `${x.uId.padStart(6, '0')} - ${x.uJmeno} ${x.uPrijmeni}`
          }))}
        />
      </Grid>
      <Grid item xs={12}>
        <SelectElement
          fullWidth control={control} name="piIdUser" label="Uživatel" required
          options={(categories?.platbyCategories?.nodes || []).map(x => ({
            id: x.pcId, label: `${x.pcId} - ${x.pcName}`
          }))}
        />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="piPrefix" label="Prefix (rok)" required />
      </Grid>
      <Grid item xs={12}>
        <Button fullWidth variant="contained" type="submit" color="primary" disabled={onSubmit.loading}>Uložit</Button>
      </Grid>

      // raw table
    </Grid>
  );
};
