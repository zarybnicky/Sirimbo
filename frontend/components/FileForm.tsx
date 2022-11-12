import { Grid } from '@mui/material';
import { FileFragment, DokumentyInput, useUpdateFileMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'react-hook-form-mui';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';

type FormProps = Pick<DokumentyInput, 'dName'>;

export const FileForm: React.FC<{
  data: FileFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doUpdate } = useUpdateFileMutation({ onSuccess });

  const { control, handleSubmit, formState } = useForm<FormProps>({
    defaultValues: {
      dName: data?.dName,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpdate({ id: data.dId, patch: values });
  });

  return (
    <Grid container spacing={1} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="dName" label="Název" required />
      </Grid>

      <Grid item xs={12}>
        <SubmitButton loading={onSubmit.loading} disabled={!formState.isValid} />
      </Grid>
    </Grid >
  );
};
