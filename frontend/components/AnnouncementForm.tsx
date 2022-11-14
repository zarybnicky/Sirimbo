import { AnnouncementFragment, UpozorneniInput, useCreateAnnouncementMutation, useUpdateAnnouncementMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextAreaElement, TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';

type FormProps = Pick<UpozorneniInput, 'upNadpis' | 'upText' | 'upLock'>;

export const AnnouncementForm: React.FC<{
  data?: AnnouncementFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateAnnouncementMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateAnnouncementMutation({ onSuccess });

  const { control, handleSubmit, formState } = useForm<FormProps>({
    defaultValues: {
      upNadpis: data?.upNadpis,
      upText: data?.upText,
      upLock: data?.upLock,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.upId, patch: values });
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
      <SubmitButton loading={onSubmit.loading} disabled={!formState.isValid} />
    </form>
  );
};
