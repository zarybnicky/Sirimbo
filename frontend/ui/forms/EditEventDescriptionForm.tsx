import { UpdateEventDocument, EventDocument } from '@/graphql/Event';
import { TabMenu } from '@/ui/TabMenu';
import { RichTextEditor } from '@/ui/fields/richtext';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  summary: z.string(),
  description: z.string(),
});

export function EditEventDescriptionForm({ id }: { id: string }) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit, getValues } = useForm({
    shouldUnregister: false,
    resolver: zodResolver(Form),
  });
  const update = useMutation(UpdateEventDocument)[1];
  const [{ data: event }] = useQuery({
    query: EventDocument,
    variables: { id },
  });
  const [tab, setTab] = React.useState('summary');

  const values = getValues();

  React.useEffect(() => {
    if (event?.event && !getValues('summary') && !getValues('description')) {
      reset({
        summary: event.event.summary,
        description: event.event.description,
      });
    }
  }, [reset, getValues, event]);

  const onSubmit = useAsyncCallback(async (values: z.infer<typeof Form>) => {
    await update({ id, patch: values });
    onSuccess();
  });

  const tabs: Array<{
    id: string;
    title: React.ReactNode;
    contents: () => React.ReactNode;
  }> = event?.event ? [
    {
      id: 'summary',
      title: 'Shrnutí',
      contents: () => (
        <RichTextEditor
          name="summary"
          initialState={values.summary || event.event?.summary}
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
          initialState={values.description || event.event?.description}
          control={control}
          key="description"
        />
      ),
    },
  ] : [];

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
