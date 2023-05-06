import { SkupinyInput } from 'lib/graphql';
import {
  CohortFragment,
  useCohortListQuery,
  useCreateCohortMutation,
  useUpdateCohortMutation,
} from 'lib/graphql/Cohorts';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { ColorPicker } from './ColorPicker';
import { useQueryClient } from '@tanstack/react-query';
import { useCohortGroupListQuery } from 'lib/graphql/CohortGroup';
import { SelectElement } from './SelectElement';
import dynamic from 'next/dynamic';
const RichTextEditor = dynamic(() => import('./RichTextEditor'), { ssr: false });
import { pick } from 'lib/form-utils';
import { pipe } from 'fp-ts/function';

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

export const CohortForm = ({ data }: { data?: CohortFragment }) => {
  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    queryClient.invalidateQueries(useCohortListQuery.getKey());
  }, [queryClient]);

  const { data: cohortGroups } = useCohortGroupListQuery();

  const { mutateAsync: doCreate } = useCreateCohortMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateCohortMutation({ onSuccess });

  const { reset, control, handleSubmit } = useForm<FormProps>({
    defaultValues: { sColorRgb: '#ff0000' }
  });
  React.useEffect(() => {
    if (data) {
      reset(pipe(data, pick(fields)));
    }
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (patch: FormProps) => {
    if (data) {
      await doUpdate({ id: data.id, patch });
    } else {
      await doCreate({ input: patch });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="sName" label="Název" required />
      <TextFieldElement control={control} name="sLocation" label="Město/místo" required />

      <div className="flex flex-wrap gap-2">
        <SelectElement
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
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
