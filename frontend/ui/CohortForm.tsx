import { CohortDocument, CreateCohortDocument, UpdateCohortDocument } from '@app/graphql/Cohorts';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from '@app/ui/fields/text';
import { CheckboxElement } from '@app/ui/fields/checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { ColorPicker } from './ColorPicker';
import { CohortGroupListDocument } from '@app/graphql/CohortGroup';
import { ComboboxElement } from './Combobox';
import { useMutation, useQuery } from 'urql';
import { toast } from 'react-toastify';
import { RichTextEditor } from '@app/ui/fields/richtext';
import { z } from 'zod';
import { makeEntityFetcher } from './generic/WithEntity';

const Form = z.object({
  sName: z.string(),
  sDescription: z.string().optional().default(''),
  sLocation: z.string().nullish(),
  sVisible: z.boolean().default(false),
  sColorRgb: z.string(),
  ordering: z.number().nullish(),
  cohortGroup: z.string().nullish(),
});
type FormProps = z.infer<typeof Form>;

export const CohortForm = ({ id = '', onSuccess }: { id?: string; onSuccess: () => void }) => {
  const [query] = useQuery({ query: CohortDocument, variables: { id }, pause: !id });
  const data = query.data?.entity;

  const [{ data: cohortGroups }] = useQuery({ query: CohortGroupListDocument });
  const create = useMutation(CreateCohortDocument)[1];
  const update = useMutation(UpdateCohortDocument)[1];

  const { reset, control, handleSubmit } = useForm<FormProps>({
    defaultValues: { sColorRgb: '#ff0000' },
  });
  React.useEffect(() => {
    reset(Form.partial().optional().parse(data));
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (patch: FormProps) => {
    if (id) {
      const res = await update({ id, patch });
      const newId = res.data?.updateSkupiny?.skupiny?.id;
      if (newId) {
        onSuccess?.();
      }
    } else {
      const res = await create({ input: patch });
      const id = res.data?.createSkupiny?.skupiny?.id;
      if (id) {
        toast.success('Přidáno.');
        onSuccess?.();
      }
    }
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />
      <ColorPicker label="Barva skupiny" name="sColorRgb" control={control} />
      <TextFieldElement control={control} name="sName" label="Název" required />
      <TextFieldElement control={control} name="sLocation" label="Město/místo" />

      <div className="flex flex-wrap gap-2">
        <ComboboxElement
          control={control}
          className="grow"
          label="Tréninkový program"
          placeholder="žádný tréninkový program"
          name="cohortGroup"
          options={(cohortGroups?.cohortGroups?.nodes || [])
            .map((x) => ({ id: x.id || null, label: x.name }))
            .concat([{ id: null, label: 'Žádný' }])}
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
        name="sVisible"
        value="1"
        label="Veřejně viditelná"
      />

      <RichTextEditor
        control={control}
        initialState={data?.sDescription}
        name="sDescription"
        label="Popis"
      />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};

CohortForm.fetcher = makeEntityFetcher(CohortDocument)((x) => x?.entity);
