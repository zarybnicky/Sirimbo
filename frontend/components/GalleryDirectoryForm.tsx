import { Grid } from '@mui/material';
import { GalleryDirFragment, GalerieDirInput, useUpdateGalleryDirMutation, useGalleryDirListQuery, useCreateGalleryDirMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { SelectElement } from 'components/SelectElement';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { slugify } from 'lib/slugify';
import { SubmitButton } from './SubmitButton';

type FormProps = Pick<GalerieDirInput, 'gdIdRodic' | 'gdName' | 'gdHidden'>;

export const GalleryDirForm: React.FC<{
  data: GalleryDirFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateGalleryDirMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateGalleryDirMutation({ onSuccess });

  const { data: dirs } = useGalleryDirListQuery();

  const { control, handleSubmit, formState } = useForm<FormProps>({
    defaultValues: {
      gdName: data?.gdName,
      gdHidden: data?.gdHidden,
      gdIdRodic: data?.gdIdRodic,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.gdId, patch: values });
    } else {
      const parent = dirs?.galerieDirs?.nodes.find(x => x.gdId === values.gdIdRodic);
      await doCreate({ input: { ...values, gdPath: parent?.gdPath + '/' + slugify(values.gdName) } });
    }
  });

  return (
    <Grid container spacing={1} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <TextFieldElement control={control} name="gdName" label="Název" required />
      </Grid>
      <Grid item xs={12}>
        <SelectElement
          control={control} name="gdIdRodic" label="Rodičovská složka" required
          options={(dirs?.galerieDirs?.nodes || []).map(x => ({
            id: x.gdId,
            label: '\xa0'.repeat(x.gdLevel - 1) + x.gdName,
          }))}
        />
      </Grid>
      <Grid item xs={12}>
        <CheckboxElement control={control} name="gdName" value="1" label="Skrytá" required />
      </Grid>

      <Grid item xs={12}>
        <SubmitButton loading={onSubmit.loading} disabled={!formState.isValid} />
      </Grid>
    </Grid >
  );
};
