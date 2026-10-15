import { DialogTitle } from '@/ui/dialog';
import {
  TenantMembershipDocument,
  UpdateTenantMembershipDocument,
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
  since: z.date(),
  until: z.date().nullish(),
});

export function EditTenantMembershipForm({ id }: { id: string }) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  const [query] = useQuery({
    query: TenantMembershipDocument,
    variables: { id },
    pause: !id,
  });
  const [result, update] = useMutation(UpdateTenantMembershipDocument);

  const item = query.data?.tenantMembership;

  React.useEffect(() => {
    if (!item) return;
    reset({
      since: item.since ? new Date(item.since) : undefined,
      until: item.until ? new Date(item.until) : undefined,
    }, {
      keepDirtyValues: true,
      keepTouched: true,
      keepErrors: true,
    });
  }, [reset, item]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    if (!item || query.error) return;
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
      <DialogTitle>Upravit členství v klubu</DialogTitle>
      <FormError error={result.error || query.error} />
      {!query.fetching && !query.error && !item && (
        <p>Záznam nebyl nalezen.</p>
      )}

      <div className="prose prose-accent">
        <h4>
          {item?.person?.name} v klubu {item?.tenantName}
        </h4>
      </div>

      <DatePickerElement control={control} name="since" label="Členství od" />
      <DatePickerElement control={control} name="until" label="Členství do" clearable />

      <div className="flex flex-wrap gap-4">
        <SubmitButton control={control} disabled={!item || !!query.error}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}
