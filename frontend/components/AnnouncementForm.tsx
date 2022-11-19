import React from 'react';
import { useForm } from 'react-hook-form';
import { TextAreaElement, TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { UpozorneniInput } from 'lib/graphql';
import { AnnouncementFragment, useAnnouncementListQuery, useCreateAnnouncementMutation, useUpdateAnnouncementMutation } from 'lib/graphql/Announcement';
import { useQueryClient } from '@tanstack/react-query';

type FormProps = Pick<UpozorneniInput, 'upNadpis' | 'upText' | 'upLock'>;

export const AnnouncementForm: React.FC<{
  data?: AnnouncementFragment;
}> = ({ data }) => {
  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    queryClient.invalidateQueries(useAnnouncementListQuery.getKey());
  }, [queryClient]);

  const { mutateAsync: doCreate } = useCreateAnnouncementMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateAnnouncementMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: {
      upNadpis: data?.upNadpis,
      upText: data?.upText,
      upLock: data?.upLock,
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
      <TextFieldElement control={control} name="upNadpis" label="Nadpis" required />
      <TextAreaElement control={control} name="upText" label="Text" rows={20} required />
      <CheckboxElement control={control} name="upLock" value="1" label="Uzamčená" />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
