import { SkupinyInput } from 'lib/graphql';
import { CohortFragment, useCohortListQuery, useCreateCohortMutation, useUpdateCohortMutation } from 'lib/graphql/Cohorts';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextAreaElement, TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { ColorPicker } from './ColorPicker';
import { useQueryClient } from '@tanstack/react-query';
import { SlateEditorElement } from './Slate';

type FormProps = Pick<SkupinyInput, 'internalInfo' | 'sName' | 'sDescription' |
  'sLocation' | 'sVisible' | 'sColorRgb' | 'ordering'>;

export const CohortForm: React.FC<{ data?: CohortFragment; }> = ({ data }) => {
  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    queryClient.invalidateQueries(useCohortListQuery.getKey());
  }, [queryClient]);

  const { mutateAsync: doCreate } = useCreateCohortMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateCohortMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: {
      sName: data?.sName,
      sDescription: data?.sDescription,
      sLocation: data?.sLocation,
      sVisible: data?.sVisible,
      sColorRgb: data?.sColorRgb || '#FF0000',
      internalInfo: data?.internalInfo,
      ordering: data?.ordering,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.id, patch: values });
    } else {
      await doCreate({ input: values });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="sName" label="Název" required />
      <TextFieldElement control={control} name="sLocation" label="Město/místo" required />
      <ColorPicker name="sColorRgb" control={control} />
      <TextAreaElement control={control} name="sDescription" label="Popis" rows={10} required />
      <SlateEditorElement control={control} name="internalInfo" label="Interní informace" />
      <CheckboxElement control={control} name="sVisible" value="1" label="Viditelná v seznamech" />
      <TextFieldElement control={control} name="ordering" label="Pořadí v seznamech skupin (1 = první, 999 = poslední)" />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
