import React from 'react';
import { GalleryDirFragment, useUpdateGalleryDirMutation, useGalleryDirListQuery, useCreateGalleryDirMutation } from 'lib/graphql/Gallery';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { SelectElement } from 'components/SelectElement';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { slugify } from 'lib/slugify';
import { SubmitButton } from './SubmitButton';
import { GalerieDirInput } from 'lib/graphql';

type FormProps = Pick<GalerieDirInput, 'gdIdRodic' | 'gdName' | 'gdHidden'>;

export const GalleryDirForm: React.FC<{
  data: GalleryDirFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateGalleryDirMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateGalleryDirMutation({ onSuccess });

  const { data: dirs } = useGalleryDirListQuery();

  const { control, handleSubmit } = useForm<FormProps>({
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
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="gdName" label="Název" required />
      <SelectElement
        control={control} name="gdIdRodic" label="Rodičovská složka" required
        options={(dirs?.galerieDirs?.nodes || []).map(x => ({
          id: x.gdId,
          label: '\xa0'.repeat(x.gdLevel - 1) + x.gdName,
        }))}
      />
      <CheckboxElement control={control} name="gdName" value="1" label="Skrytá" required />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
