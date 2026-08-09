import { CohortGroupListDocument } from '@/graphql/CohortGroup';
import {
  CohortDocument,
  CreateCohortDocument,
  UpdateCohortDocument,
} from '@/graphql/Cohorts';
import { ColorPicker } from '@/ui/fields/ColorPicker';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { RichTextEditor } from '@/ui/fields/richtext';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  name: z.string(),
  description: z.string().optional().prefault(''),
  location: z.string().nullish(),
  isVisible: z.boolean().prefault(true),
  isArchived: z.boolean().prefault(false),
  colorRgb: z.string(),
  ordering: z.number().nullish(),
  cohortGroupId: z.string().nullish(),
});

export function CohortForm({ id = '' }: { id?: string }) {
  const { onSuccess } = useFormResult();
  const [query] = useQuery({ query: CohortDocument, variables: { id }, pause: !id });
  const data = query.data?.entity;

  const [{ data: cohortGroups }] = useQuery({ query: CohortGroupListDocument });
  const [createResult, create] = useMutation(CreateCohortDocument);
  const [updateResult, update] = useMutation(UpdateCohortDocument);

  const { reset, control, handleSubmit } = useForm({
    defaultValues: { colorRgb: '#ff0000' },
    resolver: zodResolver(Form),
  });
  React.useEffect(() => {
    reset(Form.partial().optional().parse(data), {
      keepDirtyValues: true,
      keepTouched: true,
      keepErrors: true,
    });
  }, [reset, data]);

  const onSubmit = async (patch: z.infer<typeof Form>) => {
    if (id) {
      const result = await update({ id, patch });
      if (!result.error && result.data?.updateCohort?.cohort?.id) onSuccess();
    } else {
      const result = await create({ input: patch });
      if (!result.error && result.data?.createCohort?.cohort?.id) {
        toast.success('Přidáno.');
        onSuccess();
      }
    }
  };

  const programOptions = [
    ...(cohortGroups?.cohortGroups?.nodes || []).map((x) => ({
      id: x.id || null,
      label: x.name,
    })),
    { id: null, label: 'Žádný' },
  ];

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={createResult.error || updateResult.error} />
      <ColorPicker label="Barva skupiny" name="colorRgb" control={control} />
      <TextFieldElement control={control} name="name" label="Název" required />
      <TextFieldElement control={control} name="location" label="Město/místo" />

      <div className="flex flex-wrap gap-2">
        <ComboboxElement
          control={control}
          className="grow"
          label="Tréninkový program"
          placeholder="žádný tréninkový program"
          name="cohortGroupId"
          options={programOptions}
        />
        <TextFieldElement
          control={control}
          className="grow"
          type="number"
          name="ordering"
          label="Pořadí v seznamech skupin (1 = první)"
        />
      </div>
      <CheckboxElement
        control={control}
        name="isVisible"
        value="1"
        label="Veřejně viditelná"
      />
      <CheckboxElement
        control={control}
        name="isArchived"
        value="1"
        label="Archivovaná"
      />

      <RichTextEditor
        control={control}
        initialState={data?.description}
        name="description"
        label="Popis"
      />
      <SubmitButton control={control} />
    </form>
  );
}
