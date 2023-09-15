import { useZodForm } from '@/lib/use-schema-form';
import { DeleteTenantTrainerDocument, TenantTrainerDocument, TenantTrainerFragment, UpdateTenantTrainerDocument } from '@app/graphql/Memberships';
import { useConfirm } from '@app/ui/Confirm';
import { Dialog, DialogContent } from '@app/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@app/ui/dropdown';
import { DatePickerElement } from '@app/ui/fields/date';
import { formatOpenDateRange } from '@app/ui/format';
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

const Form = z.object({
  since: z.date().nullish().default(null),
  until: z.date().nullish().default(null),
  defaultPrice: z.number().optional().nullish().default(null),
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
        defaultPrice: item.defaultPrice?.amount,
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
          defaultPrice: {
            amount: values.defaultPrice,
            currency: 'CZK',
          },
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
      <TextFieldElement control={control} type="number" name="defaultPrice" label="Cena za 45min" />

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
  }, [update]);

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
            <span>
              {formatOpenDateRange(data)}
              {perms.isAdmin && (
                data.defaultPrice?.amount ? `, ${data.defaultPrice.amount}Kč/45min` : ''
              )}
            </span>
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
