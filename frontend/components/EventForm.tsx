import { Grid } from '@mui/material';
import { EventFragment, AkceInput, useCreateEventMutation, useUpdateEventMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextAreaElement, TextFieldElement } from 'components/TextField';
import { DatePickerElement } from 'react-hook-form-mui';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';

type FormProps = Pick<AkceInput, 'aJmeno' | 'aKde' | 'aInfo' | 'aOd' | 'aDo' |
  'aKapacita' | 'aVisible' | 'aLock'>;

export const EventForm: React.FC<{
  data?: EventFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateEventMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateEventMutation({ onSuccess });

  const { control, handleSubmit, formState } = useForm<FormProps>({
    defaultValues: {
      aJmeno: data?.aJmeno,
      aKde: data?.aKde,
      aInfo: data?.aInfo,
      aOd: data?.aOd,
      aDo: data?.aDo,
      aKapacita: data?.aKapacita,
      aVisible: data?.aVisible,
      aLock: data?.aLock,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.aId, patch: values });
    } else {
      await doCreate({ input: { ...values, aDokumenty: '' } });
    }
  });

  return (
    <Grid container spacing={1.5} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <TextFieldElement control={control} name="aJmeno" label="Název" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement control={control} name="aKde" label="Místo akce" required />
      </Grid>
      <Grid item xs={12}>
        <TextAreaElement control={control} name="aInfo" label="Další info" rows={3} required />
      </Grid>
      <Grid item xs={12}>
        <DatePickerElement inputProps={{ fullWidth: true }} control={control} label="Od" name="aOd" required />
      </Grid>
      <Grid item xs={12}>
        <DatePickerElement inputProps={{ fullWidth: true }} helperText="(pokud je prázdné, počítá se jako 'Od')" control={control} label="Do" name="aDo" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement control={control} type="number" name="aKapacita" label="Kapacita" required />
      </Grid>
      <Grid item xs={12}>
        <CheckboxElement control={control} name="aVisible" value="1" label="Zviditelnit" />
      </Grid>
      <Grid item xs={12}>
        <CheckboxElement control={control} name="aLock" value="1" label="Uzamčená" />
      </Grid>
      <Grid item xs={12}>
        <SubmitButton loading={onSubmit.loading} disabled={!formState.isValid} />
      </Grid>
    </Grid>
  );
};
