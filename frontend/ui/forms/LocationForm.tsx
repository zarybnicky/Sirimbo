import { LocationDocument, UpsertLocationDocument } from '@/graphql/Location';
import { FileListDocument } from '@/graphql/File';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { TextField, TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { FilePicker } from '@/ui/forms/FilePicker';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useController, useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  name: z.string(),
  description: z.string().nullish(),
  isPublic: z.boolean().prefault(false),
  imageIds: z.array(z.string()).prefault([]),
  coverImageId: z.string().nullish(),
  address: z
    .object({
      city: z.string().nullish(),
      conscriptionNumber: z.string().nullish(),
      district: z.string().nullish(),
      orientationNumber: z.string().nullish(),
      postalCode: z.string().nullish(),
      region: z.string().nullish(),
      street: z.string().nullish(),
    })
    .nullish(),
});

export function LocationForm({ id = '' }: { id?: string }) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues: { imageIds: [], coverImageId: null },
  });
  const [query] = useQuery({
    query: LocationDocument,
    variables: { id },
    pause: !id,
  });
  const [{ data: fileData }] = useQuery({ query: FileListDocument });
  const [result, upsert] = useMutation(UpsertLocationDocument);
  const imageIds = useController({ control, name: 'imageIds' }).field;
  const coverImageId = useController({ control, name: 'coverImageId' }).field;

  const item = query.data?.location;
  const filesById = new Map(
    [
      ...(fileData?.files?.nodes ?? []),
      ...(item?.imagesList.flatMap((x) => (x.file ? [x.file] : [])) ?? []),
    ].map((file) => [file.id, file]),
  );
  const coverOptions = (imageIds.value ?? []).flatMap((id) => {
    const file = filesById.get(id);
    return file ? [{ id, label: file.displayName ?? file.name }] : [];
  });

  React.useEffect(() => {
    if (!item) return;
    reset(
      {
        name: item.name,
        description: item.description,
        isPublic: item.isPublic,
        imageIds: item.imagesList.map((x) => x.fileId),
        coverImageId: item.imagesList.some((x) => x.fileId === item.coverImageId)
          ? item.coverImageId
          : null,
        address: {
          street: item.address?.street || '',
          conscriptionNumber: item.address?.conscriptionNumber || '',
          orientationNumber: item.address?.orientationNumber || '',
          district: item.address?.district || '',
          city: item.address?.city || '',
          postalCode: item.address?.postalCode || '',
          region: item.address?.region || '',
        },
      },
      {
        keepDirtyValues: true,
        keepTouched: true,
        keepErrors: true,
      },
    );
  }, [reset, item]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const { imageIds, coverImageId, ...location } = values;
    if (!Object.values(location.address || {}).some(Boolean)) {
      location.address = null;
    }
    const saved = await upsert({
      input: {
        details: { id: id || undefined, ...location },
        imageIds,
        coverImageId,
      },
    });
    if (!saved.error) onSuccess();
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error} />

      <TextFieldElement control={control} name="name" label="Jméno" />
      <TextFieldElement control={control} name="description" label="Popis" />

      <div className="grid gap-2 md:grid-cols-[2fr_1fr_1fr]">
        <TextFieldElement control={control} name="address.street" label="Ulice" />
        <TextFieldElement
          control={control}
          name="address.conscriptionNumber"
          label="Č. popisné"
        />
        <TextFieldElement
          control={control}
          name="address.orientationNumber"
          label="Č. orientační"
        />
      </div>

      <div className="grid gap-2 md:grid-cols-[1fr_1fr]">
        <TextFieldElement control={control} name="address.district" label="Část města" />
        <TextFieldElement control={control} name="address.city" label="Město" />
      </div>

      <TextFieldElement control={control} name="address.postalCode" label="PSČ" />
      <TextFieldElement control={control} name="address.region" label="Kraj" />
      <TextField label="Země" value="Česká republika" disabled />

      <CheckboxElement control={control} name="isPublic" label="Veřejné" />

      <FilePicker
        value={imageIds.value ?? []}
        onChange={(value) => {
          imageIds.onChange(value);
          if (coverImageId.value && !value.includes(coverImageId.value)) {
            coverImageId.onChange(null);
          }
        }}
        imagesOnly
        title="Fotografie"
      />

      {coverOptions.length > 0 && (
        <ComboboxElement
          control={control}
          name="coverImageId"
          label="Úvodní fotografie"
          placeholder="Bez úvodní fotografie"
          options={coverOptions}
          helperText="Vyberte z fotografií místa."
        />
      )}

      <div className="flex flex-wrap gap-4">
        <SubmitButton control={control}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}
