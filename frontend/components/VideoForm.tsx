import { Button, Grid } from '@mui/material';
import { VideoFragment, VideoInput, useCreateVideoMutation, useUpdateVideoMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'react-hook-form-mui';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';

type FormProps = Pick<VideoInput, 'vUri' | 'vTitle' | 'vDescription' | 'vPlaylist' | 'vAuthor'>;

export const VideoForm: React.FC<{
  data?: VideoFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateVideoMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateVideoMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: {
      vUri: data?.vUri,
      vTitle: data?.vTitle,
      vDescription: data?.vDescription,
      vPlaylist: data?.vPlaylist,
      vAuthor: data?.vAuthor,
    },
  });

  // $split = preg_split("/(vi\/|v=|\/v\/|youtu\.be\/|\/embed\/)/", $uri);
  // $query = ($split && isset($split[1])) ? $split[1] : $uri;
  // $query = preg_split("/[^0-9a-z_\-]/i", $query);
  // return $query[0];

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (!data) {
      await doCreate({ input: values });
    } else {
      await doUpdate({ id: data.vId, patch: values });
    }
  });

  return (
    <Grid container spacing={1.5} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="vUri" required label={<>
          ID videa (např. <code>https://www.youtube.com/watch?v=<b>lt6h7Fgxohs</b></code>)<br />
          Ostatní pole se později zjistí z YouTube, pokud zůstanou prázdná.
        </>} />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="vTitle" label="Jméno" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="vAuthor" label="Autor/kanál" required />
      </Grid>
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="vPlaylist" label="ID playlistu (nepovinné)" required />
      </Grid>
      <Grid item xs={12}>
        <Button fullWidth variant="contained" type="submit" color="primary" disabled={onSubmit.loading}>Uložit</Button>
      </Grid>
    </Grid>
  );
};
