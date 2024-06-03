import { UpdateUserProxyDocument, UserProxyDocument } from '@/graphql/Memberships';
import { useZodForm } from '@/lib/use-schema-form';
import { DatePickerElement } from '@/ui/fields/date';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { type TypeOf, z } from 'zod';

const Form = z.object({
  since: z.date().nullish(),
  until: z.date().nullish(),
});

export function EditUserProxyForm({ id }: { id: string }) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit } = useZodForm(Form);
  const [query] = useQuery({ query: UserProxyDocument, variables: { id }, pause: !id });
  const update = useMutation(UpdateUserProxyDocument)[1];

  const item = query.data?.userProxy

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
          since: values.since ? values.since.toISOString() : null,
          until: values.until ? values.until.toISOString() : null,
        },
      },
    });
    onSuccess();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <div>{item?.user?.uEmail}, {item?.user?.uLogin}</div>
      <div><b>Přístupové údaje pro osobu {item?.person?.name}</b></div>

      <DatePickerElement control={control} name="since" label="Platné od" />
      <DatePickerElement control={control} name="until" label="Platné do" />

      <div className="flex flex-wrap gap-4">
        <SubmitButton loading={onSubmit.loading}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}
