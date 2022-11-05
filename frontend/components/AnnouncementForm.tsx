import { Button, Grid } from '@mui/material';
import { AnnouncementFragment, AktualityInput, useCreateAnnouncementMutation, useUpdateAnnouncementMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'react-hook-form-mui';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';

type FormProps = Pick<AktualityInput, 'atJmeno' | 'atPreview' | 'atText'>;

export const AnnouncementForm: React.FC<{
  data?: AnnouncementFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateAnnouncementMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateAnnouncementMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: {
      atJmeno: data?.atJmeno,
      atPreview: data?.atPreview,
      atText: data?.atText,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.atId, patch: values });
    } else {
      await doCreate({ input: { ...values, atKat: '1' } });
    }
  });

  return (
    <Grid container spacing={3} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="atJmeno" label="Název" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="atPreview" label="Shrnutí" rows={3} multiline required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="atText" label="Text" rows={20} multiline required />
      </Grid>
      <Grid item xs={12}>
        <Button fullWidth variant="contained" type="submit" color="primary" disabled={onSubmit.loading}>Uložit</Button>
      </Grid>
    </Grid>
  );
};
