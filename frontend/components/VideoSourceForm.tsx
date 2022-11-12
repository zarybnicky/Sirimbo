import { Grid } from '@mui/material';
import { VideoSourceFragment, VideoSourceInput, useCreateVideoSourceMutation, useUpdateVideoSourceMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';

type FormProps = Pick<VideoSourceInput, 'vsUrl' | 'vsTitle' | 'vsDescription'>;

export const VideoSourceForm: React.FC<{
  data?: VideoSourceFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateVideoSourceMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateVideoSourceMutation({ onSuccess });

  const { control, handleSubmit, formState } = useForm<FormProps>({
    defaultValues: {
      vsUrl: data?.vsUrl,
      vsTitle: data?.vsTitle,
      vsDescription: data?.vsDescription,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (!data) {
      await doCreate({ input: values });
    } else {
      await doUpdate({ id: data.vsId, patch: values });
    }
  });

  return (
    <Grid container spacing={1.5} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <TextFieldElement control={control} name="vsUrl" required label={<>
          ID kanálu (např. <code>https://www.youtube.com/channel/<b>UCopG139AfgpmaswNXmEwX2Q</b></code>)<br />
          Ostatní pole se později zjistí z YouTube, pokud zůstanou prázdná.
        </>} />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement control={control} name="vsTitle" label="Jméno" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement control={control} name="vsDescription" label="Popis" required />
      </Grid>
      <Grid item xs={12}>
        <SubmitButton loading={onSubmit.loading} disabled={!formState.isValid} />
      </Grid>
    </Grid>
  );
};
