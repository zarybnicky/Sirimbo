import {
  EventDescriptionDocument,
  UpdateEventDocument,
} from '@/graphql/Event';
import { TabMenu } from '@/ui/TabMenu';
import { RichTextEditor } from '@/ui/fields/richtext';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  summary: z.string(),
  description: z.string(),
});

export function EditEventInstanceDescriptionForm({ id }: { id: string }) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit, getValues } = useForm({
    shouldUnregister: false,
    resolver: zodResolver(Form),
  });
  const [result, update] = useMutation(UpdateEventDocument);
  const [{ data }] = useQuery({
    query: EventDescriptionDocument,
    variables: { id },
  });
  const [tab, setTab] = React.useState('summary');
  const instance = data?.eventInstance;
  const values = getValues();

  React.useEffect(() => {
    if (instance && !getValues('summary') && !getValues('description')) {
      reset({
        summary: instance.summary ?? '',
        description: instance.description ?? '',
      });
    }
  }, [reset, getValues, instance]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const result = await update({ id, patch: values });
    if (!result.error) onSuccess();
  };

  const tabs: Array<{
    id: string;
    title: React.ReactNode;
    contents: () => React.ReactNode;
  }> = instance
    ? [
        {
          id: 'summary',
          title: 'Shrnutí',
          contents: () => (
            <RichTextEditor
              name="summary"
              initialState={values.summary || instance.summary || ''}
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
              initialState={values.description || instance.description || ''}
              control={control}
              key="description"
            />
          ),
        },
      ]
    : [];

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error} />

      <div className="max-w-full">
        <TabMenu selected={tab} onSelect={setTab} options={tabs} />
      </div>

      <div className="flex flex-wrap gap-4">
        <SubmitButton control={control}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}
