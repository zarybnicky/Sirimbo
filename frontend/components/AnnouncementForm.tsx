import { Grid } from '@mui/material';
import { AnnouncementFragment, UpozorneniInput, useCreateAnnouncementMutation, useUpdateAnnouncementMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'react-hook-form-mui';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';

type FormProps = Pick<UpozorneniInput, 'upNadpis' | 'upText' | 'upLock'>;

export const AnnouncementForm: React.FC<{
  data?: AnnouncementFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateAnnouncementMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateAnnouncementMutation({ onSuccess });

  const { control, handleSubmit, formState } = useForm<FormProps>({
    defaultValues: {
      upNadpis: data?.upNadpis,
      upText: data?.upText,
      upLock: data?.upLock,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.upId, patch: values });
    } else {
      await doCreate({ input: values });
    }
  });

  return (
    <Grid container spacing={1.5} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="upNadpis" label="Nadpis" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="upText" label="Text" rows={20} multiline required />
      </Grid>
      <Grid item xs={12}>
        <CheckboxElement control={control} name="upLock" value="1" label="Uzamčená" />
      </Grid>
      <Grid item xs={12}>
        <SubmitButton loading={onSubmit.loading} disabled={!formState.isValid} />
      </Grid>
    </Grid>
  );
};
