import { Alert, Button, Grid } from '@mui/material';
import { ArticleFragment, AktualityInput, useCreateArticleMutation, useUpdateArticleMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'react-hook-form-mui';

type FormProps = AktualityInput;

export const ArticleForm: React.FC<{
  data?: ArticleFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateArticleMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateArticleMutation({ onSuccess });

  const [submitError, setSubmitError] = React.useState<string | null>(null);
  const { control, handleSubmit } = useForm<FormProps>({ defaultValues: data });
  const onSubmit = async (values: FormProps) => {
    setSubmitError(null);
    try {
      await (data ? doUpdate({
        id: data.atId,
        patch: {
          atJmeno: values.atJmeno,
          atPreview: values.atPreview,
          atText: values.atText,
        },
      }) : doCreate({
        input: {
          ...values,
          atKat: '1',
        },
      }));
    } catch (e) {
      setSubmitError(e instanceof Error ? e.message : 'Něco se nepovedlo');
    }
  };

  return (
    <Grid container spacing={3} component="form" onSubmit={handleSubmit(onSubmit)}>
      <Grid item xs={12}>
        {submitError && <Alert severity="error">{submitError}</Alert>}
        <TextFieldElement fullWidth control={control} name="atJmeno" label="Název" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="atPreview" label="Shrnutí" rows={3} multiline required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="atText" label="Text" rows={20} multiline required />
      </Grid>
      <Grid item xs={12}>
        <Button fullWidth variant="contained" type="submit" color="primary">Uložit</Button>
      </Grid>
    </Grid>
  );
};
