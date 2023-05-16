import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { UpozorneniInput } from 'lib/graphql';
import {
  AnnouncementFragment,
  AnnouncementListDocument,
  CreateAnnouncementDocument,
  MyAnnouncementsDocument,
  UpdateAnnouncementDocument,
} from 'lib/graphql/Announcement';
import { useQueryClient } from '@tanstack/react-query';
import { DateRange, DateRangeInput } from './DateRange';
import dynamic from 'next/dynamic';
import { getGqlKey, useGqlMutation } from 'lib/query';
import { CheckboxElement } from './Checkbox';
const RichTextEditor = dynamic(() => import('./RichTextEditor'), { ssr: false });

type FormProps = Pick<UpozorneniInput, 'upNadpis' | 'upText' | 'isVisible'> & {
  schedule: DateRange;
};

export const AnnouncementForm: React.FC<{
  data?: AnnouncementFragment;
}> = ({ data }) => {
  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    queryClient.invalidateQueries(getGqlKey(MyAnnouncementsDocument, {}));
    queryClient.invalidateQueries(getGqlKey(AnnouncementListDocument, {}));
  }, [queryClient]);

  const { mutateAsync: doCreate } = useGqlMutation(CreateAnnouncementDocument, {
    onSuccess,
  });
  const { mutateAsync: doUpdate } = useGqlMutation(UpdateAnnouncementDocument, {
    onSuccess,
  });

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      upNadpis: data?.upNadpis,
      upText: data?.upText,
      isVisible: data?.isVisible,
      schedule: [
        data?.scheduledSince ? new Date(data.scheduledSince) : undefined,
        data?.scheduledUntil ? new Date(data.scheduledUntil) : undefined,
      ],
    });
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const patch = {
      upNadpis: values.upNadpis,
      upText: values.upText,
      isVisible: values.isVisible,
      scheduledSince: values.schedule[0]?.toISOString(),
      scheduledUntil: values.schedule[1]?.toDateString(),
    };
    if (data) {
      await doUpdate({ id: data.id, patch });
    } else {
      await doCreate({ input: patch });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="upNadpis" label="Nadpis" required />
      <DateRangeInput control={control} name="schedule" label="Publikováno od/do" />
      <CheckboxElement control={control} name="isVisible" value="1" label="Viditelný" />
      <RichTextEditor
        initialState={data?.upText}
        control={control}
        name="upText"
        label="Text"
      />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
