import { SkupinyInput } from '@app/graphql';
import {
  CohortDocument,
  CreateCohortDocument,
  DeleteCohortDocument,
  UpdateCohortDocument,
} from '@app/graphql/Cohorts';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { ColorPicker } from './ColorPicker';
import { CohortGroupListDocument } from '@app/graphql/CohortGroup';
import { ComboboxElement } from './Combobox';
import { pick } from 'lib/form-utils';
import { pipe } from 'fp-ts/function';
import { useMutation, useQuery } from 'urql';
import { DeleteButton } from './DeleteButton';
import { useRouter } from 'next/router';
import { Route } from 'nextjs-routes';
import { ErrorPage } from './ErrorPage';
import { toast } from 'react-toastify';
import { RichTextEditor } from './RichTextEditor';
import { TitleBar } from './layout/TitleBar';

const fields = [
  'sName',
  'sDescription',
  'sLocation',
  'sVisible',
  'sColorRgb',
  'internalInfo',
  'ordering',
  'cohortGroup',
] as const;
type FormProps = Pick<SkupinyInput, (typeof fields)[number]>;

const backHref: Route = {pathname: '/admin/skupiny' };

export const CohortForm = ({ id = '' }: { id?: string }) => {
  const router = useRouter();
  const [query] = useQuery({query: CohortDocument, variables: { id }, pause: !!id });
  const data = query.data?.skupiny;
  const title = id ? (data?.sName || '(Bez názvu)') : 'Nová skupina';

  const [{ data: cohortGroups }] = useQuery({query: CohortGroupListDocument });
  const create = useMutation(CreateCohortDocument)[1];
  const update = useMutation(UpdateCohortDocument)[1];

  const { reset, control, handleSubmit } = useForm<FormProps>({
    defaultValues: { sColorRgb: '#ff0000' },
  });
  React.useEffect(() => {
    if (data) {
      reset(pipe(data, pick(fields))); //TODO: replace
    }
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (patch: FormProps) => {
    if (id) {
      await update({ id, patch });
    } else {
      const res = await create({ input: patch });
      const id = res.data?.createSkupiny?.skupiny?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace({ pathname: '/admin/skupiny/[id]', query: { id } });
      } else {
        reset(undefined);
      }
    }
  });

  if (query.data && query.data.skupiny === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TitleBar backHref={backHref} title={title}>
        <DeleteButton
          doc={DeleteCohortDocument}
          id={id}
          onDelete={() => router.push('/admin/skupiny')}
          title="smazat skupinu"
        />
        <SubmitButton loading={onSubmit.loading} />
      </TitleBar>

      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="sName" label="Název" required />
      <TextFieldElement control={control} name="sLocation" label="Město/místo" required />

      <div className="flex flex-wrap gap-2">
        <ComboboxElement
          control={control}
          className="grow"
          label="Tréninkový program"
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
