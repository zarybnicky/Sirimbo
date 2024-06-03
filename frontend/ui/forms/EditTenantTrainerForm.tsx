import { TenantTrainerDocument, UpdateTenantTrainerDocument } from '@/graphql/Memberships';
import { useZodForm } from '@/lib/use-schema-form';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { DatePickerElement } from '@/ui/fields/date';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { type TypeOf, z } from 'zod';

const Form = z.object({
  since: z.date().nullish().default(null),
  until: z.date().nullish().default(null),
  memberPrice: z.number().optional().nullish().default(null),
  guestPrice: z.number().optional().nullish().default(null),
  memberPayout: z.number().optional().nullish().default(null),
  guestPayout: z.number().optional().nullish().default(null),
  createPayoutPayments: z.boolean().optional().default(true),
});

export function EditTenantTrainerForm({ id }: { id: string }) {
  const { onSuccess } = useFormResult();
  const { reset, control, handleSubmit } = useZodForm(Form);
  const [query] = useQuery({ query: TenantTrainerDocument, variables: { id }, pause: !id });
  const update = useMutation(UpdateTenantTrainerDocument)[1];

  const item = query.data?.tenantTrainer

  React.useEffect(() => {
    if (item) {
      reset({
        since: item.since ? new Date(item.since) : null,
        until: item.until ? new Date(item.until) : null,
        memberPrice: Number.parseFloat(item.memberPrice45Min?.amount || '0') || null,
        guestPrice: Number.parseFloat(item.guestPrice45Min?.amount || '0') || null,
        memberPayout: Number.parseFloat(item.memberPayout45Min?.amount || '0') || null,
        guestPayout: Number.parseFloat(item.guestPayout45Min?.amount || '0') || null,
        createPayoutPayments: item.createPayoutPayments,
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
          memberPrice45Min: values.memberPrice ? {amount: values.memberPrice, currency: 'CZK'} : null,
          guestPrice45Min: values.guestPrice ? {amount: values.guestPrice, currency: 'CZK'} : null,
          memberPayout45Min: values.memberPayout ? {amount: values.memberPayout, currency: 'CZK'} : null,
          guestPayout45Min: values.guestPayout ? {amount: values.guestPayout, currency: 'CZK'} : null,
          createPayoutPayments: values.createPayoutPayments,
        },
      },
    });
    onSuccess();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <div className="prose">
        <h4>Trenér {item?.person?.name} v klubu {item?.tenant?.name}</h4>
      </div>

      <DatePickerElement control={control} name="since" label="Trenér od" />
      <DatePickerElement control={control} name="until" label="Trenér do" />
      <div className="grid lg:grid-cols-2 gap-2">
        <TextFieldElement control={control} type="number" name="memberPrice" label="Cena pro členy (Kč/45min)" />
        <TextFieldElement control={control} type="number" name="guestPrice" label="Cena pro hosty (Kč/45min)" />
      </div>
      <CheckboxElement control={control} name="createPayoutPayments" label="Vyplácet na kreditní účet člena" />
      <div className="grid lg:grid-cols-2 gap-2">
        <TextFieldElement control={control} type="number" name="memberPayout" label="Vypláceno (členové, Kč/45min)" />
        <TextFieldElement control={control} type="number" name="guestPayout" label="Vypláceno (hosté, Kč/45min)" />
      </div>

      <div className="flex flex-wrap gap-4">
        <SubmitButton loading={onSubmit.loading}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}
