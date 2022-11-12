import { Grid } from '@mui/material';
import { CohortFragment, SkupinyInput, useCreateCohortMutation, useUpdateCohortMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextAreaElement, TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { ColorPicker } from './ColorPicker';

type FormProps = Pick<SkupinyInput, 'sName' | 'sDescription' | 'sLocation' | 'sVisible' | 'sColorRgb'>;

export const CohortForm: React.FC<{
  data?: CohortFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateCohortMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateCohortMutation({ onSuccess });

  const { control, handleSubmit, formState } = useForm<FormProps>({
    defaultValues: {
      sName: data?.sName,
      sDescription: data?.sDescription,
      sLocation: data?.sLocation,
      sVisible: data?.sVisible,
      sColorRgb: data?.sColorRgb || '#FF0000',
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
        <TextFieldElement control={control} name="sName" label="Název" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement control={control} name="sLocation" label="Město/místo" required />
      </Grid>
      <Grid item xs={12}>
        <CheckboxElement control={control} name="sVisible" value="1" label="Viditelná pro registraci" />
      </Grid>
      <Grid item xs={12}>
        <ColorPicker name="sColorRgb" control={control} />
      </Grid>
      <Grid item xs={12}>
        <TextAreaElement control={control} name="sDescription" label="Popis" rows={3} required />
      </Grid>
      <Grid item xs={12}>
        <SubmitButton loading={onSubmit.loading} disabled={!formState.isValid} />
      </Grid>
    </Grid >
  );
};
