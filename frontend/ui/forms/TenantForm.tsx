import { TenantFormDocument, UpdateTenantDocument } from '@/graphql/Tenant';
import { RichTextEditor } from '@/ui/fields/richtext';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  name: z.string(),
  bankAccount: z.string(),
  description: z.string(),
  cstsId: z.string().nullable(),
});

export function TenantForm() {
  const [{ data, error }] = useQuery({ query: TenantFormDocument });
  const { onSuccess } = useFormResult();
  const [result, update] = useMutation(UpdateTenantDocument);

  const { reset, control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  React.useEffect(() => {
    reset(
      data?.tenant
        ? { ...data.tenant, cstsId: data.tenant.cstsId?.toString() ?? null }
        : undefined,
      {
        keepDirtyValues: true,
        keepTouched: true,
        keepErrors: true,
      },
    );
  }, [reset, data?.tenant]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    if (!data) return;
    const result = await update({
      input: {
        id: data.tenant?.id!,
        patch: { ...values, cstsId: values.cstsId ? Number(values.cstsId) : null },
      },
    });
    if (!result.error) onSuccess();
  };

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error ?? error} />
      <TextFieldElement control={control} name="name" label="Název organizace" required />
      <ComboboxElement
        control={control}
        name="cstsId"
        label="Klub ČSTS"
        options={data?.clubs?.nodes.map((club) => ({
          id: club.externalId,
          label: `${club.name} (${club.externalId})`,
        }))}
      />
      <TextFieldElement
        control={control}
        name="bankAccount"
        label="Číslo účtu"
        required
      />
      <RichTextEditor control={control} name="description" label="Základní informace" />
      <SubmitButton control={control} />
    </form>
  );
}
