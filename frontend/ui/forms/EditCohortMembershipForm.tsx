import {
  CohortMembershipDocument,
  UpdateCohortMembershipDocument,
} from '@/graphql/Memberships';
import { DatePickerElement } from '@/ui/fields/date';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  since: z.date({ error: 'Zadejte začátek členství' }),
  until: z.date().nullish(),
});

export function EditCohortMembershipForm({ id }: { id: string }) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit } = useForm({
    mode: 'onBlur',
    resolver: zodResolver(Form),
  });
  const [query] = useQuery({
    query: CohortMembershipDocument,
    variables: { id },
    pause: !id,
  });
  const [result, update] = useMutation(UpdateCohortMembershipDocument);

  const item = query.data?.cohortMembership;

  React.useEffect(() => {
    if (item) {
      reset({
        since: item.since ? new Date(item.since) : undefined,
        until: item.until ? new Date(item.until) : undefined,
      });
    }
  }, [reset, item]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    const result = await update({
      input: {
        id,
        patch: {
          since: values.since.toISOString(),
          until: values.until ? values.until.toISOString() : null,
        },
      },
    });
    if (!result.error) onSuccess();
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error} />

      <div>
        <b>
          {item?.person?.name} ve skupině {item?.cohort?.name}
        </b>
      </div>

      <DatePickerElement control={control} name="since" label="Členství od" />
      <DatePickerElement control={control} name="until" label="Členství do" clearable />

      <div className="flex flex-wrap gap-4">
        <SubmitButton control={control}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}
