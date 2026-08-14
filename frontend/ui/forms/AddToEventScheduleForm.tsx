import { zodResolver } from '@hookform/resolvers/zod';
import React from 'react';
import { useForm } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { EventListDocument, UpdateEventDocument } from '@/graphql/Event';
import { DialogTitle } from '@/ui/dialog';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { FormError, useFormResult } from '@/ui/form';
import { formatEventName, fullDateFormatter } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';

const Form = z.object({
  parentId: z.string().min(1, 'Vyberte událost'),
});

export function AddToEventScheduleForm({
  eventId,
}: {
  eventId: string;
}) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit } = useForm<z.infer<typeof Form>>({
    resolver: zodResolver(Form),
  });
  const [updateResult, update] = useMutation(UpdateEventDocument);
  const [{ data, error, fetching }] = useQuery({ query: EventListDocument });
  const options = React.useMemo(
    () =>
      (data?.eventInstancesList ?? [])
        .filter((event) => event.id !== eventId)
        .map((event) => ({
          id: event.id,
          label: `${formatEventName(event)} - ${fullDateFormatter.formatRange(
            new Date(event.since),
            new Date(event.until),
          )}`,
        })),
    [data, eventId],
  );
  const onSubmit = async (values: z.infer<typeof Form>) => {
    const result = await update({
      id: eventId,
      patch: { parentId: values.parentId },
    });
    if (!result.error) onSuccess();
  };

  return (
    <form className="grid gap-4" onSubmit={handleSubmit(onSubmit)}>
      <DialogTitle>Přidat do rozpisu události</DialogTitle>
      <FormError error={error || updateResult.error} />
      <ComboboxElement
        control={control}
        name="parentId"
        label="Událost"
        placeholder={fetching ? 'Načítám události…' : 'Vyberte událost'}
        options={options}
      />
      <SubmitButton control={control} disabled={fetching} />
    </form>
  );
}
