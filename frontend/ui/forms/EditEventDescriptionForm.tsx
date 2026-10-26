import { EventDescriptionDocument, UpdateEventDocument } from '@/graphql/Event';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { RichTextEditor } from '@/ui/fields/richtext';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { FilePicker } from '@/ui/forms/FilePicker';
import { useController, useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  coverImageId: z.string().nullable(),
  summary: z.string(),
  description: z.string(),
});

export function EditEventDescriptionForm({ id }: { id: string }) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit } = useForm({
    shouldUnregister: false,
    defaultValues: { summary: '', description: '', coverImageId: null },
    resolver: zodResolver(Form),
  });
  const [result, update] = useMutation(UpdateEventDocument);
  const [{ data }] = useQuery({ query: EventDescriptionDocument, variables: { id } });
  const [tab, setTab] = React.useState('summary');
  const instance = data?.event;
  const coverImage = useController({ control, name: 'coverImageId' }).field;

  React.useEffect(() => {
    if (!instance) return;
    reset(
      {
        coverImageId: instance.coverImageId,
        summary: instance.summary ?? '',
        description: instance.description ?? '',
      },
      {
        keepDirtyValues: true,
        keepTouched: true,
        keepErrors: true,
      },
    );
  }, [reset, instance]);

  const onSubmit = async (patch: z.infer<typeof Form>) => {
    const result = await update({ id, patch });
    if (!result.error) onSuccess();
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error} />

      {instance && (
        <FilePicker
          title="Úvodní obrázek"
          imagesOnly
          multiple={false}
          value={coverImage.value ? [coverImage.value] : []}
          onChange={(ids) => coverImage.onChange(ids[0] ?? null)}
        />
      )}

      <div className="max-w-full">
        <TabMenu selected={tab} onSelect={setTab}>
          {instance && (
            <>
              <Tab id="summary" title="Shrnutí">
                <RichTextEditor
                  name="summary"
                  control={control}
                />
              </Tab>
              <Tab id="description" title="Dlouhý popis">
                <RichTextEditor
                  name="description"
                  control={control}
                />
              </Tab>
            </>
          )}
        </TabMenu>
      </div>

      <div className="flex flex-wrap gap-4">
        <SubmitButton control={control}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}
