import { Grid } from '@mui/material';
import { ScheduleFragment, RozpiInput, useCreateScheduleMutation, useUpdateScheduleMutation, useTrainerListQuery } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { DatePickerElement, SelectElement, TextFieldElement } from 'react-hook-form-mui';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';

type FormProps = Pick<RozpiInput, 'rTrener' | 'rKde' | 'rDatum' | 'rVisible' | 'rLock'>;

export const ScheduleForm: React.FC<{
  data?: ScheduleFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateScheduleMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateScheduleMutation({ onSuccess });

  const { data: trainers } = useTrainerListQuery();

  const { control, handleSubmit, formState } = useForm<FormProps>({
    defaultValues: {
      rKde: data?.rKde,
      rDatum: data?.rDatum,
      rTrener: data?.rTrener,
      rVisible: data?.rVisible,
      rLock: data?.rLock,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.rId, patch: values });
    } else {
      await doCreate({ input: values });
    }
  });

  return (
    <Grid container spacing={1.5} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <SelectElement
          fullWidth control={control} name="rTrener" label="Trenér" required
          options={(trainers?.trainers?.nodes || []).map(x => ({ id: x.uId, label: `${x.uJmeno} ${x.uPrijmeni}` }))}
        />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="rKde" label="Místo" required />
      </Grid>
      <Grid item xs={12}>
        <DatePickerElement inputProps={{ fullWidth: true }} control={control} label="Datum" name="rDatum" required />
      </Grid>
      <Grid item xs={12}>
        <CheckboxElement control={control} name="rVisible" value="1" label="Viditelný" />
      </Grid>
      <Grid item xs={12}>
        <CheckboxElement control={control} name="rLock" value="1" label="Uzamčený" />
      </Grid>
      <Grid item xs={12}>
        <SubmitButton loading={onSubmit.loading} disabled={!formState.isValid} />
      </Grid>
    </Grid>
  );
};
