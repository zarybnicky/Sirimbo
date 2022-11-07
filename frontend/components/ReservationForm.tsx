import { Button, Grid } from '@mui/material';
import { ReservationFragment, NabidkaInput, useCreateReservationMutation, useUpdateReservationMutation } from 'lib/graphql';
import React from 'react';
import { Controller, useForm } from 'react-hook-form';
import { CheckboxElement, TextFieldElement } from 'react-hook-form-mui';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { ColorFormat, ColorPicker } from 'mui-color';

type FormProps = Pick<NabidkaInput, 'nTrener' | 'nPocetHod' | 'nMaxPocetHod' | 'nOd' | 'nDo' | 'nVisible' | 'nLock'>;

export const ReservationForm: React.FC<{
  data?: ReservationFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateReservationMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateReservationMutation({ onSuccess });

  // trainers - if admin, then all with pe_nabidka >= OWNED, else [me]

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: {
      nTrener: data?.nTrener,
      nPocetHod: data?.nPocetHod,
      nMaxPocetHod: data?.nMaxPocetHod,
      nOd: data?.nOd,
      nDo: data?.nDo,
      nVisible: data?.nVisible,
      nLock: data?.nLock,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.nId, patch: values });
    } else {
      await doCreate({ input: { ...values } });
    }
  });

  return (
    <Grid container spacing={1.5} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="sName" label="Název" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="sLocation" label="Město/místo" required />
      </Grid>
      <Grid item xs={12}>
        <CheckboxElement control={control} name="sVisible" value="1" label="Viditelná pro registraci" />
      </Grid>
      <Grid item xs={12}>
        <Controller
          name="sColorRgb"
          control={control}
          defaultValue="#FF0000"
          render={({ field: { onChange, value } }) => (
            <ColorPicker
              inputFormats={["hex" as any as ColorFormat]}
              value={value} onChange={(v) => onChange((v as any).css.backgroundColor)} />
          )}
        />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="sDescription" label="Popis" rows={3} multiline required />
      </Grid>
      <Grid item xs={12}>
        <Button fullWidth variant="contained" type="submit" color="primary" disabled={onSubmit.loading}>Uložit</Button>
      </Grid>
    </Grid >
  );
};
