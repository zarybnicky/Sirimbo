import { Alert, Button, Grid } from '@mui/material';
import { ArticleFragment, AktualityInput, useCreateArticleMutation, useUpdateArticleMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'react-hook-form-mui';
import { useAsyncCallback } from 'react-async-hook'

type FormProps = Pick<AktualityInput, 'atJmeno' | 'atPreview' | 'atText'>;

export const ArticleForm: React.FC<{
  data?: ArticleFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateArticleMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateArticleMutation({ onSuccess });

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
      doCreate({ input: { ...values, atKat: '1' } });
    }
    onSuccess();
  });

  return (
    <Grid container spacing={3} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <Grid item xs={12}>
        {onSubmit.error && <Alert severity="error">{onSubmit.error}</Alert>}
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
