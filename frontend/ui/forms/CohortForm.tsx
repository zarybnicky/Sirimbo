import { CohortGroupListDocument } from '@/graphql/CohortGroup';
import { CohortDocument, CreateCohortDocument, UpdateCohortDocument } from '@/graphql/Cohorts';
import { useZodForm } from '@/lib/use-schema-form';
import { ColorPicker } from '@/ui/fields/ColorPicker';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { RichTextEditor } from '@/ui/fields/richtext';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { type TypeOf, z } from 'zod';

const Form = z.object({
  name: z.string(),
  description: z.string().optional().default(''),
  location: z.string().nullish(),
  isVisible: z.boolean().default(false),
  colorRgb: z.string(),
  ordering: z.number().nullish(),
  cohortGroupId: z.string().nullish(),
});

export function CohortForm({ id = '' }: { id?: string }) {
  const { onSuccess } = useFormResult();
  const [query] = useQuery({ query: CohortDocument, variables: { id }, pause: !id });
  const data = query.data?.entity;

  const [{ data: cohortGroups }] = useQuery({ query: CohortGroupListDocument });
  const create = useMutation(CreateCohortDocument)[1];
  const update = useMutation(UpdateCohortDocument)[1];

  const { reset, control, handleSubmit } = useZodForm(Form, {
    defaultValues: { colorRgb: '#ff0000' },
  });
  React.useEffect(() => {
    reset(Form.partial().optional().parse(data));
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (patch: TypeOf<typeof Form>) => {
    if (id) {
      const res = await update({ id, patch });
      const newId = res.data?.updateCohort?.cohort?.id;
      if (newId) {
        onSuccess();
      }
    } else {
      const res = await create({ input: patch });
      const id = res.data?.createCohort?.cohort?.id;
      if (id) {
        toast.success('Přidáno.');
        onSuccess();
      }
    }
  });

  const programOptions = [
    ...(cohortGroups?.cohortGroups?.nodes || []).map((x) => ({ id: x.id || null, label: x.name })),
    { id: null, label: 'Žádný' }
  ];

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />
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

      <RichTextEditor
        control={control}
        initialState={data?.description}
        name="description"
        label="Popis"
      />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}
