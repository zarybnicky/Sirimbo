import { type EventFragment, UpdateEventDocument } from '@/graphql/Event';
import { useZodForm } from '@/lib/use-schema-form';
import { TabMenu } from '@/ui/TabMenu';
import { RichTextEditor } from '@/ui/fields/richtext';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation } from 'urql';
import { type TypeOf, z } from 'zod';

const Form = z.object({
  summary: z.string(),
  description: z.string(),
  descriptionMember: z.string(),
});

type NonEmptyArray<T> = [T, ...T[]];

export function EditEventDescriptionForm({ event }: { event: EventFragment }) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit, getValues } = useZodForm(Form, {
    shouldUnregister: false,
  });
  const update = useMutation(UpdateEventDocument)[1];
  const [tab, setTab] = React.useState('summary');

  const values = getValues();

  React.useEffect(() => {
    if (!getValues('summary') && !getValues('description') && !getValues('descriptionMember')) {
      reset({
        summary: event.summary,
        description: event.description,
        descriptionMember: event.descriptionMember,
      });
    }
  }, [reset, getValues, event]);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    await update({ id: event.id, patch: values });
    onSuccess();
  });

  const tabs: NonEmptyArray<{ id: string; label: React.ReactNode, contents: React.ReactNode; }> = [
    {
      id: 'summary',
      label: 'Shrnutí',
      contents: (
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
      label: 'Dlouhý popis',
      contents: (
        <RichTextEditor
          name="description"
          initialState={values.description || event?.description}
          control={control}
          key="description"
        />
      ),
    },
    {
      id: 'descriptionMember',
      label: 'Další info jen pro členy',
      contents: (
        <RichTextEditor
          name="descriptionMember"
          initialState={values.descriptionMember || event?.descriptionMember}
          control={control}
          key="descriptionMember"
        />
      ),
    },
  ];

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <TabMenu selected={tab || tabs[0].id} onSelect={setTab} options={tabs} />
      <div className="mt-2 relative max-w-full">
        {(tabs.find((x) => x.id === tab) || tabs[0]).contents}
      </div>

      <div className="flex flex-wrap gap-4">
        <SubmitButton loading={onSubmit.loading}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}
