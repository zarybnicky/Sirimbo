import { CreateTenantLocationDocument, TenantLocationDocument, UpdateTenantLocationDocument } from '@/graphql/Tenant';
import { useZodForm } from '@/lib/use-schema-form';
import { tenantId } from '@/tenant/config';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { TextField, TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { type TypeOf, z } from 'zod';

const Form = z.object({
  name: z.string(),
  description: z.string().nullish(),
  isPublic: z.boolean(),
  address: z.object({
    city: z.string().nullish(),
    conscriptionNumber: z.string().nullish(),
    district: z.string().nullish(),
    orientationNumber: z.string().nullish(),
    postalCode: z.string().nullish(),
    region: z.string().nullish(),
    street: z.string().nullish(),
  }),
});

export function EditTenantLocationForm({ id = '' }: { id?: string }) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit } = useZodForm(Form);
  const [query] = useQuery({ query: TenantLocationDocument, variables: { id }, pause: !id });
  const create = useMutation(CreateTenantLocationDocument)[1];
  const update = useMutation(UpdateTenantLocationDocument)[1];

  const item = query.data?.tenantLocation;

  React.useEffect(() => {
    if (item) {
      reset({
        name: item.name,
        description: item.description,
        isPublic: !!item.isPublic,
        address: {
          street: item.address?.street || '',
          conscriptionNumber: item.address?.conscriptionNumber || '',
          orientationNumber: item.address?.orientationNumber || '',
          district: item.address?.district || '',
          city: item.address?.city || '',
          postalCode: item.address?.postalCode || '',
          region: item.address?.region || '',
        },
      });
    }
  }, [reset, item]);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    if (id) {
      await update({ input: { id, patch: { ...values, isPublic: !!values.isPublic } } });
    } else {
      await create({ input: { tenantLocation: { ...values, isPublic: !!values.isPublic, tenantId } } });
    }
    onSuccess();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <TextFieldElement control={control} name="name" label="Jméno" />
      <TextFieldElement control={control} name="description" label="Popis" />

      <div className="grid gap-2 md:grid-cols-[2fr_1fr_1fr]">
        <TextFieldElement control={control} name="address.street" label="Ulice" />
        <TextFieldElement control={control} name="address.conscriptionNumber" label="Č. popisné" />
        <TextFieldElement control={control} name="address.orientationNumber" label="Č. orientační" />
      </div>

      <div className="grid gap-2 md:grid-cols-[1fr_1fr]">
        <TextFieldElement control={control} name="address.district" label="Část města" />
        <TextFieldElement control={control} name="address.city" label="Město" />
      </div>

      <TextFieldElement control={control} name="address.postalCode" label="PSČ" />
      <TextFieldElement control={control} name="address.region" label="Kraj" />
      <TextField label="Země" value="Česká republika" disabled />

      <CheckboxElement control={control} name="isPublic" label="Veřejné" />

      <div className="flex flex-wrap gap-4">
        <SubmitButton loading={onSubmit.loading}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}
