import { TenantDocument, UpdateTenantDocument } from '@/graphql/Tenant';
import { useAuth } from '@/lib/auth';
import { RichTextEditor } from '@/ui/fields/richtext';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { TextFieldElement } from '@/ui/fields/text';
import { DialogTitle } from '@/ui/dialog';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  name: z.string().min(1, 'Název je povinný'),
  bankAccount: z.string(),
  description: z.string(),
  cstsId: z.string().nullable(),
  czIco: z.string(),
  czDic: z.string(),
  origins: z.string(),
  address: z
    .object({
      street: z.string().nullish(),
      conscriptionNumber: z.string().nullish(),
      orientationNumber: z.string().nullish(),
      district: z.string().nullish(),
      city: z.string().nullish(),
      region: z.string().nullish(),
      postalCode: z.string().nullish(),
    })
    .nullable(),
});

export function TenantForm({ tenantId }: { tenantId: string }) {
  const auth = useAuth();
  const [query] = useQuery({
    query: TenantDocument,
    variables: { tenantId },
  });
  const { onSuccess } = useFormResult();
  const [result, update] = useMutation(UpdateTenantDocument);

  const { reset, control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
  });
  const tenant = query.data?.tenant;
  React.useEffect(() => {
    if (!tenant) return;
    reset(
      {
        ...tenant,
        cstsId: tenant.cstsId?.toString() ?? null,
        origins: tenant.origins.join(', '),
      },
      {
        keepDirtyValues: true,
        keepTouched: true,
        keepErrors: true,
      },
    );
  }, [reset, tenant]);

  const onSubmit = async (values: z.infer<typeof Form>) => {
    if (!tenant || query.error) return;
    const { origins, ...details } = values;
    const result = await update({
      input: {
        id: tenantId,
        patch: {
          ...details,
          ...(auth.isSystemAdmin && {
            origins: origins
              .split(',')
              .map((origin) => origin.trim())
              .filter(Boolean),
          }),
          cstsId: values.cstsId ? Number(values.cstsId) : null,
          address: Object.values(values.address ?? {}).some(Boolean)
            ? values.address
            : null,
        },
      },
    });
    if (!result.error) onSuccess();
  };

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit)}>
      <DialogTitle>Upravit klub</DialogTitle>
      <FormError error={result.error ?? query.error} />
      <TextFieldElement control={control} name="name" label="Název organizace" required />
      <ComboboxElement
        control={control}
        name="cstsId"
        label="Klub ČSTS"
        options={query.data?.clubs?.nodes.map((club) => ({
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
      <TextFieldElement
        control={control}
        name="origins"
        label="Domény (oddělené čárkou)"
        readOnly={!auth.isSystemAdmin}
      />
      <div className="grid gap-2 sm:grid-cols-2">
        <TextFieldElement control={control} name="czIco" label="IČO" />
        <TextFieldElement control={control} name="czDic" label="DIČ" />
      </div>
      <fieldset className="grid gap-2">
        <legend className="mb-2 font-semibold">Adresa klubu</legend>
        <div className="grid gap-2 sm:grid-cols-[2fr_1fr_1fr]">
          <TextFieldElement control={control} name="address.street" label="Ulice" />
          <TextFieldElement
            control={control}
            name="address.conscriptionNumber"
            label="Č. popisné"
          />
          <TextFieldElement
            control={control}
            name="address.orientationNumber"
            label="Č. orientační"
          />
        </div>
        <div className="grid gap-2 sm:grid-cols-2">
          <TextFieldElement
            control={control}
            name="address.district"
            label="Část města"
          />
          <TextFieldElement control={control} name="address.city" label="Město" />
          <TextFieldElement control={control} name="address.postalCode" label="PSČ" />
          <TextFieldElement control={control} name="address.region" label="Kraj" />
        </div>
      </fieldset>
      <SubmitButton control={control} disabled={!tenant || !!query.error} />
    </form>
  );
}
