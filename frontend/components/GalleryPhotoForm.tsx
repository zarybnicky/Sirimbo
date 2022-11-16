import { GalleryPhotoFragment, GalerieFotoInput, useUpdateGalleryPhotoMutation, useGalleryDirListQuery } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { SelectElement } from 'components/SelectElement';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';

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
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="gfName" label="Název" required />
      <SelectElement
        control={control} name="gfIdRodic" label="Rodičovská složka" required
        options={(dirs?.galerieDirs?.nodes || []).map(x => ({
          id: x.gdId,
          label: '\xa0'.repeat(x.gdLevel - 1) + x.gdName,
        }))}
      />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
