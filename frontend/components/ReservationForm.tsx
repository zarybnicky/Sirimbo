import { Grid } from '@mui/material';
import { ReservationFragment, NabidkaInput, useCreateReservationMutation, useUpdateReservationMutation, useTrainerListQuery } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { DatePickerElement } from 'react-hook-form-mui';
import { SelectElement } from 'components/SelectElement';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';

type FormProps = Pick<NabidkaInput, 'nTrener' | 'nPocetHod' | 'nMaxPocetHod' | 'nOd' | 'nDo' | 'nVisible' | 'nLock'>;

export const ReservationForm: React.FC<{
  data?: ReservationFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateReservationMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateReservationMutation({ onSuccess });

  const { data: trainers } = useTrainerListQuery();

  const { control, handleSubmit, formState } = useForm<FormProps>({
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
      await doCreate({ input: values });
    }
  });

  return (
    <Grid container spacing={1.5} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <SelectElement
          control={control} name="nTrener" label="Trenér" required
          options={(trainers?.trainers?.nodes || []).map(x => ({ id: x.uId, label: `${x.uJmeno} ${x.uPrijmeni}` }))}
        />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement control={control} name="nPocetHod" label="Počet hodin" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement control={control} name="nMaxPocetHod" label="Max.počet hodin" required />
      </Grid>
      <Grid item xs={12}>
        <DatePickerElement inputProps={{ fullWidth: true }} control={control} label="Od" name="nOd" required />
      </Grid>
      <Grid item xs={12}>
        <DatePickerElement inputProps={{ fullWidth: true }} control={control} label="Do" name="nDo" required />
      </Grid>
      <Grid item xs={12}>
        <CheckboxElement control={control} name="nVisible" value="1" label="Viditelný" />
      </Grid>
      <Grid item xs={12}>
        <CheckboxElement control={control} name="nLock" value="1" label="Uzamčený" />
      </Grid>
      <Grid item xs={12}>
        <SubmitButton loading={onSubmit.loading} disabled={!formState.isValid} />
      </Grid>
    </Grid >
  );
};
