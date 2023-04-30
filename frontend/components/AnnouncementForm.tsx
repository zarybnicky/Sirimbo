import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { UpozorneniInput } from 'lib/graphql';
import {
  AnnouncementFragment,
  useAnnouncementListQuery,
  useCreateAnnouncementMutation,
  useUpdateAnnouncementMutation,
} from 'lib/graphql/Announcement';
import { useQueryClient } from '@tanstack/react-query';
import { DateRange, DateRangeInput } from './DateRange';
import { SlateEditorElement } from './Slate';

type FormProps = Pick<UpozorneniInput, 'upNadpis' | 'upText'> & {
  schedule: DateRange;
};

export const AnnouncementForm: React.FC<{
  data?: AnnouncementFragment;
}> = ({ data }) => {
  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    queryClient.invalidateQueries(useAnnouncementListQuery.getKey());
  }, [queryClient]);

  const { mutateAsync: doCreate } = useCreateAnnouncementMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateAnnouncementMutation({ onSuccess });

  const { reset, control, handleSubmit } = useForm<FormProps>();
  const [iter, setIter] = React.useState(0);
  React.useEffect(() => {
    reset({
      upNadpis: data?.upNadpis,
      upText: data?.upText,
      schedule: [
        data?.scheduledSince ? new Date(data.scheduledSince) : undefined,
        data?.scheduledUntil ? new Date(data.scheduledUntil) : undefined,
      ],
    });
    setIter(x => x + 1);
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const patch = {
      upNadpis: values.upNadpis,
      upText: JSON.stringify(values.upText),
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
      <SlateEditorElement control={control} iter={iter} name="upText" label="Text" />
      <DateRangeInput control={control} name="schedule" label="Publikováno od/do" />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
