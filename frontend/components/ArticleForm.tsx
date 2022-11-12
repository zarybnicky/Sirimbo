import { Grid } from '@mui/material';
import { ArticleFragment, AktualityInput, useCreateArticleMutation, useUpdateArticleMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextAreaElement, TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';

type FormProps = Pick<AktualityInput, 'atJmeno' | 'atPreview' | 'atText'>;

export const ArticleForm: React.FC<{
  data?: ArticleFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateArticleMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateArticleMutation({ onSuccess });

  const { control, handleSubmit, formState } = useForm<FormProps>({
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
    <Grid container spacing={1.5} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <TextFieldElement control={control} name="atJmeno" label="Název" required />
      </Grid>
      <Grid item xs={12}>
        <TextAreaElement control={control} name="atPreview" label="Shrnutí" rows={3} required />
      </Grid>
      <Grid item xs={12}>
        <TextAreaElement control={control} name="atText" label="Text" rows={20} required />
      </Grid>
      <Grid item xs={12}>
        <SubmitButton loading={onSubmit.loading} disabled={!formState.isValid} />
      </Grid>
    </Grid>
  );
};
