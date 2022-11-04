import { Button, Grid } from '@mui/material';
import { VideoSourceFragment, VideoSourceInput, useCreateVideoSourceMutation, useUpdateVideoSourceMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'react-hook-form-mui';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';

type FormProps = Pick<VideoSourceInput, 'vsUrl' | 'vsTitle' | 'vsDescription'>;

export const VideoSourceForm: React.FC<{
  data?: VideoSourceFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateVideoSourceMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateVideoSourceMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>({
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
    onSuccess();
  });

  return (
    <Grid container spacing={3} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="vsUrl" required label={<>
          ID kanálu (např. <code>https://www.youtube.com/channel/<b>UCopG139AfgpmaswNXmEwX2Q</b></code>)<br />
          Ostatní pole se později zjistí z YouTube, pokud zůstanou prázdná.
        </>} />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="vsTitle" label="Jméno" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="vsDescription" label="Popis" required />
      </Grid>
      <Grid item xs={12}>
        <Button fullWidth variant="contained" type="submit" color="primary" disabled={onSubmit.loading}>Uložit</Button>
      </Grid>
    </Grid>
  );
};
