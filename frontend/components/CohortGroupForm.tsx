import {
  CohortGroupFragment,
  useCohortGroupListQuery,
  useCreateCohortGroupMutation,
  useUpdateCohortGroupMutation,
} from 'lib/graphql/CohortGroup';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { useQueryClient } from '@tanstack/react-query';
import { CohortGroupInput } from 'lib/graphql';
import { toast } from 'react-toastify';
import dynamic from 'next/dynamic';
const RichTextEditor = dynamic(() => import('./RichTextEditor'), { ssr: false });

type FormProps = Pick<CohortGroupInput, 'name' | 'description' | 'isPublic' | 'ordering'>;

export const CohortGroupForm: React.FC<{ data?: CohortGroupFragment }> = ({ data }) => {
  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    queryClient.invalidateQueries(useCohortGroupListQuery.getKey());
  }, [queryClient]);

  const { mutateAsync: doCreate } = useCreateCohortGroupMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateCohortGroupMutation({ onSuccess });

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      name: data?.name || '',
      description: data?.description,
      isPublic: data?.isPublic || false,
      ordering: data?.ordering || 0,
    }, { keepDirtyValues: true });
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (patch: FormProps) => {
    if (data) {
      await doUpdate({ id: data.id, patch });
    } else {
      await doCreate({ input: patch });
      toast.success('Přidáno.');
      reset(undefined, { keepValues: false });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="name" label="Název" required />
      <RichTextEditor control={control} initialState={data?.description} name="description" label="Popis" />
      <CheckboxElement control={control} name="isPublic" label="Zobrazit pro veřejnost" />
      <TextFieldElement
        control={control}
        type="number"
        name="ordering"
        label="Pořadí v seznamech skupin (1 = první, 999 = poslední)"
      />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
