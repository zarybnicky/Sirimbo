import { UpdateUserProxyDocument, UserProxyDocument } from '@/graphql/Memberships';
import { DatePickerElement } from '@/ui/fields/date';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  since: z.date().nullish(),
  until: z.date().nullish(),
});

export function EditUserProxyForm({ id }: { id: string }) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  const [query] = useQuery({ query: UserProxyDocument, variables: { id }, pause: !id });
  const [result, update] = useMutation(UpdateUserProxyDocument);

  const item = query.data?.userProxy;

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
          since: values.since ? values.since.toISOString() : null,
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
        {item?.user?.uEmail}, {item?.user?.uLogin}
      </div>
      <div>
        <b>Přístupové údaje pro osobu {item?.person?.name}</b>
      </div>

      <DatePickerElement control={control} name="since" label="Platné od" clearable />
      <DatePickerElement control={control} name="until" label="Platné do" clearable />

      <div className="flex flex-wrap gap-4">
        <SubmitButton control={control} disabled={!item}>
          Uložit změny
        </SubmitButton>
      </div>
    </form>
  );
}
