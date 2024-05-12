import { CoupleDocument, UpdateCoupleDocument } from '@/graphql/Memberships';
import { useZodForm } from '@/lib/use-schema-form';
import { DatePickerElement } from '@/ui/fields/date';
import { FormError } from '@/ui/form';
import { formatLongCoupleName } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { TypeOf, z } from 'zod';

const Form = z.object({
  since: z.date().nullish().default(null),
  until: z.date().nullish().default(null),
});

export function EditCoupleForm({ id, onSuccess }: { id: string; onSuccess: () => void }) {
  const { reset, control, handleSubmit } = useZodForm(Form);
  const [query] = useQuery({ query: CoupleDocument, variables: { id }, pause: !id });
  const update = useMutation(UpdateCoupleDocument)[1];

  const item = query.data?.couple;

  React.useEffect(() => {
    if (item) {
      reset({
        since: item.since ? new Date(item.since) : null,
        until: item.until ? new Date(item.until) : null,
      });
    }
  }, [reset, item]);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    await update({
      input: {
        id,
        patch: {
          since: values.since ? values.since.toISOString() : null,
          until: values.until ? values.until.toISOString() : null,
        },
      },
    });
    onSuccess?.();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <div>
        <b>{formatLongCoupleName(item)}</b>
      </div>

      <DatePickerElement control={control} name="since" label="Od" />
      <DatePickerElement control={control} name="until" label="Do" />

      <div className="flex flex-wrap gap-4">
        <SubmitButton loading={onSubmit.loading}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}
