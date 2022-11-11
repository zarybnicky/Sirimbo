import { TextField, Button, Grid } from '@mui/material';
import { CohortFragment, SkupinyInput, useCreateCohortMutation, useUpdateCohortMutation } from 'lib/graphql';
import React from 'react';
import { Controller, useForm } from 'react-hook-form';
import { CheckboxElement, TextFieldElement } from 'react-hook-form-mui';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { HexColorPicker } from 'react-colorful';

type FormProps = Pick<SkupinyInput, 'sName' | 'sDescription' | 'sLocation' | 'sVisible' | 'sColorRgb'>;

export const CohortForm: React.FC<{
  data?: CohortFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateCohortMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateCohortMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: {
      sName: data?.sName,
      sDescription: data?.sDescription,
      sLocation: data?.sLocation,
      sVisible: data?.sVisible,
      sColorRgb: data?.sColorRgb,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.sId, patch: values });
    } else {
      await doCreate({ input: { ...values, sColorText: '' } });
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
          name="sColorRgb" control={control} defaultValue="#FF0000"
          render={({ field: { onChange, value } }) => <>
            <HexColorPicker color={value} onChange={onChange} />
            <TextField name="sColorRgb" value={value} onChange={onChange} />
          </>}
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
