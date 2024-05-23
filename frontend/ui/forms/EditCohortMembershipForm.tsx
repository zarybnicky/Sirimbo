import { CohortMembershipDocument, UpdateCohortMembershipDocument } from '@/graphql/Memberships';
import { useZodForm } from '@/lib/use-schema-form';
import { DatePickerElement } from '@/ui/fields/date';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { TypeOf, z } from 'zod';

const Form = z.object({
  since: z.date(),
  until: z.date().nullish(),
});

export function EditCohortMembershipForm({ id }: { id: string }) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit } = useZodForm(Form);
  const [query] = useQuery({ query: CohortMembershipDocument, variables: { id }, pause: !id });
  const update = useMutation(UpdateCohortMembershipDocument)[1];

  const item = query.data?.cohortMembership

  React.useEffect(() => {
    if (item) {
      reset({
        since: item.since ? new Date(item.since) : undefined,
        until: item.until ? new Date(item.until) : undefined,
      });
    }
  }, [reset, item]);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    await update({
      input: {
        id,
        patch: {
          since: values.since.toISOString(),
          until: values.until ? values.until.toISOString() : null,
        },
      },
    });
    onSuccess();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <div>{item?.tenant?.name}</div>
      <div><b>{item?.person?.name} ve skupině {item?.cohort?.name}</b></div>

      <DatePickerElement control={control} name="since" label="Členství od" />
      <DatePickerElement control={control} name="until" label="Členství do" />

      <div className="flex flex-wrap gap-4">
        <SubmitButton loading={onSubmit.loading}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}
