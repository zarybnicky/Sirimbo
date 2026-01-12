import { type EventFragment, UpdateEventDocument } from '@/graphql/Event';
import { TabMenu } from '@/ui/TabMenu';
import { RichTextEditor } from '@/ui/fields/richtext';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  summary: z.string(),
  description: z.string(),
});

type NonEmptyArray<T> = [T, ...T[]];

export function EditEventDescriptionForm({ event }: { event: EventFragment }) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit, getValues } = useForm({
    shouldUnregister: false,
    resolver: zodResolver(Form),
  });
  const update = useMutation(UpdateEventDocument)[1];
  const [tab, setTab] = React.useState('summary');

  const values = getValues();

  React.useEffect(() => {
    if (!getValues('summary') && !getValues('description')) {
      reset({
        summary: event.summary,
        description: event.description,
      });
    }
  }, [reset, getValues, event]);

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    await update({ id: event.id, patch: values });
    onSuccess();
  });

  const tabs: NonEmptyArray<{
    id: string;
    title: React.ReactNode;
    contents: () => React.ReactNode;
  }> = [
    {
      id: 'summary',
      title: 'Shrnutí',
      contents: () => (
        <RichTextEditor
          name="summary"
          initialState={values.summary || event?.summary}
          control={control}
          key="summary"
        />
      ),
    },
    {
      id: 'description',
      title: 'Dlouhý popis',
      contents: () => (
        <RichTextEditor
          name="description"
          initialState={values.description || event?.description}
          control={control}
          key="description"
        />
      ),
    },
  ];

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <div className="max-w-full">
        <TabMenu selected={tab} onSelect={setTab} options={tabs} />
      </div>

      <div className="flex flex-wrap gap-4">
        <SubmitButton loading={onSubmit.loading}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}
