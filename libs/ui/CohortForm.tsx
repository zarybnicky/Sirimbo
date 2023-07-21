import {
  CohortDocument,
  CreateCohortDocument,
  DeleteCohortDocument,
  UpdateCohortDocument,
} from '@app/graphql/Cohorts';
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
import { DeleteButton } from './DeleteButton';
import { useRouter } from 'next/router';
import { ErrorPage } from './ErrorPage';
import { toast } from 'react-toastify';
import { RichTextEditor } from '@app/ui/fields/richtext';
import { TitleBar } from './TitleBar';
import { z } from 'zod';
import { AdminEntity } from './generic/AdminEntityList';
import { makeEntityFetcher } from './generic/WithEntity';

const Form = z.object({
  sName: z.string(),
  sDescription: z.string(),
  sLocation: z.string().optional(),
  sVisible: z.boolean().default(false),
  sColorRgb: z.string(),
  internalInfo: z.string().optional(),
  ordering: z.number().optional(),
  cohortGroup: z.string().optional(),
});
type FormProps = z.infer<typeof Form>;

export const CohortForm = ({ entity, id = '' }: { entity: AdminEntity; id?: string }) => {
  const router = useRouter();
  const [query] = useQuery({ query: CohortDocument, variables: { id }, pause: !!id });
  const data = query.data?.entity;
  const title = id ? data?.sName || '(Bez názvu)' : 'Nová skupina';

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
      await update({ id, patch });
    } else {
      const res = await create({ input: patch });
      const id = res.data?.createSkupiny?.skupiny?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace(entity.editRoute(id));
      } else {
        reset(undefined);
      }
    }
  });

  if (query.data && query.data.entity === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TitleBar backHref={entity.listRoute} title={title}>
        <DeleteButton
          doc={DeleteCohortDocument}
          id={id}
          redirect={entity.listRoute}
          title="smazat skupinu"
        />
        <SubmitButton loading={onSubmit.loading} />
      </TitleBar>

      <FormError error={onSubmit.error} />
      <TextFieldElement control={control} name="sName" label="Název" required />
      <TextFieldElement control={control} name="sLocation" label="Město/místo" required />

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
        label="Viditelná v seznamech"
      />

      <ColorPicker label="Barva skupiny" name="sColorRgb" control={control} />
      <RichTextEditor
        control={control}
        initialState={data?.sDescription}
        name="sDescription"
        label="Popis"
      />
      <RichTextEditor
        control={control}
        initialState={data?.internalInfo}
        name="internalInfo"
        label="Interní informace"
      />
    </form>
  );
};

CohortForm.fetcher = makeEntityFetcher(CohortDocument)((x) => x?.entity);
