import { Button, Grid } from '@mui/material';
import { GalleryPhotoFragment, GalerieFotoInput, useUpdateGalleryPhotoMutation, useGalleryDirListQuery } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { SelectElement, TextFieldElement } from 'react-hook-form-mui';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';

type FormProps = Pick<GalerieFotoInput, 'gfIdRodic' | 'gfName'>;

export const GalleryPhotoForm: React.FC<{
  data: GalleryPhotoFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doUpdate } = useUpdateGalleryPhotoMutation({ onSuccess });

  const { data: dirs } = useGalleryDirListQuery();

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: {
      gfName: data?.gfName,
      gfIdRodic: data?.gfIdRodic,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpdate({ id: data.gfId, patch: values });
  });

  return (
    <Grid container spacing={1} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="gfName" label="Název" required />
      </Grid>
      <Grid item xs={12}>
        <SelectElement
          fullWidth control={control} name="gfIdRodic" label="Rodičovská složka" required
          options={(dirs?.galerieDirs?.nodes || []).map(x => ({
            id: x.gdId,
            label: '\xa0'.repeat(x.gdLevel - 1) + x.gdName,
          }))}
        />
      </Grid>

      <Grid item xs={12}>
        <Button fullWidth variant="contained" type="submit" color="primary" disabled={onSubmit.loading}>Uložit</Button>
      </Grid>
    </Grid >
  );
};