import { useZodForm } from '@/lib/use-schema-form';
import { DeleteTenantTrainerDocument, TenantTrainerDocument, TenantTrainerFragment, UpdateTenantTrainerDocument } from '@app/graphql/Memberships';
import { useConfirm } from '@app/ui/Confirm';
import { Dialog, DialogContent } from '@app/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@app/ui/dropdown';
import { DatePickerElement } from '@app/ui/fields/date';
import { MoreHorizontal } from 'lucide-react';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { TypeOf, z } from 'zod';
import { FormError } from './form';
import { SubmitButton } from './submit';
import { useAuth } from './use-auth';
import Link from 'next/link';
import { TextFieldElement } from './fields/text';
import { CheckboxElement } from './fields/checkbox';

const Form = z.object({
  since: z.date().nullish().default(null),
  until: z.date().nullish().default(null),
  memberPrice: z.number().optional().nullish().default(null),
  guestPrice: z.number().optional().nullish().default(null),
  memberPayout: z.number().optional().nullish().default(null),
  guestPayout: z.number().optional().nullish().default(null),
  createPayoutPayments: z.boolean().optional().default(true),
});

export function EditTenantTrainerForm({ id, onSuccess }: { id: string; onSuccess: () => void }) {
  const { reset, control, handleSubmit } = useZodForm(Form);
  const [query] = useQuery({ query: TenantTrainerDocument, variables: { id }, pause: !id });
  const update = useMutation(UpdateTenantTrainerDocument)[1];

  const item = query.data?.tenantTrainer

  React.useEffect(() => {
    if (item) {
      reset({
        since: item.since ? new Date(item.since) : null,
        until: item.until ? new Date(item.until) : null,
        memberPrice: parseFloat(item.memberPrice45Min?.amount || '0') || null,
        guestPrice: parseFloat(item.guestPrice45Min?.amount || '0') || null,
        memberPayout: parseFloat(item.memberPayout45Min?.amount || '0') || null,
        guestPayout: parseFloat(item.guestPayout45Min?.amount || '0') || null,
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
    onSuccess?.();
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

export function EditTenantTrainerCard({ data, showPerson }: { data: TenantTrainerFragment; showPerson?: boolean }) {
  const { perms } = useAuth();
  const [editOpen, setEditOpen] = React.useState(false);
  const update = useMutation(UpdateTenantTrainerDocument)[1];
  const del = useMutation(DeleteTenantTrainerDocument)[1];
  const confirm = useConfirm();

  const endToday = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete ${data.person?.name} ukončit trenérství ke dnešnímu datu?` })
    await update({ input: { id: data.id, patch: { until: new Date().toISOString() } }});
    toast.success("Ukončeno");
  }, [update, data.id, confirm, data.person?.name]);

  return (
    <>
      <DropdownMenu key={data.id}>
        <div className="flex gap-3 mb-1 align-baseline">
          {perms.isAdmin && (
            <DropdownMenuTrigger>
              <MoreHorizontal className="w-5 h-5 text-neutral-10" />
            </DropdownMenuTrigger>
          )}

          <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            {showPerson ? (
              <Link className="underline font-bold" href={`/clenove/${data.person?.id}`}>{data.person?.name}</Link>
            ) : (
              <b>Trenér v klubu {data.tenant?.name}</b>
            )}
            <div className="flex flex-wrap gap-4">
              {perms.isAdmin && (
                <span>
                  {data.memberPrice45Min?.amount ?? '- '}
                  {'Kč '}
                  {data.guestPrice45Min ? ('(' + data.guestPrice45Min.amount + 'Kč)') : ''}
                  {' / 45min'}
                </span>
              )}
            </div>
          </div>
        </div>

        <DropdownMenuContent align="start">
          {perms.isAdmin && (
            <DropdownMenuButton onClick={() => setEditOpen(true)}>Upravit trenéra</DropdownMenuButton>
          )}
          {perms.isAdmin && (
            <DropdownMenuButton onClick={() => endToday()}>Ukončit ke dnešnímu datu</DropdownMenuButton>
          )}
          {perms.isAdmin && (
            <DropdownMenuButton onClick={async () => {
              await confirm({ description: `Opravdu chcete trenéra NENÁVRATNĚ smazat, včetně všech odučených lekcí? Spíše použij variantu ukončení členství, ať zůstanou zachována historická data.` })
              await del({ id: data.id });
              toast.success("Odstraněno");
            }}>Smazat</DropdownMenuButton>
          )}
        </DropdownMenuContent>
      </DropdownMenu>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent>
          <EditTenantTrainerForm id={data.id} onSuccess={() => setEditOpen(false)} />
        </DialogContent>
      </Dialog>
    </>
  );
}
