import { useZodForm } from '@/lib/use-schema-form';
import { DeleteTenantMembershipDocument, TenantMembershipDocument, TenantMembershipFragment, UpdateTenantMembershipDocument } from '@app/graphql/Memberships';
import { useConfirm } from '@app/ui/Confirm';
import { Dialog, DialogContent } from '@app/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuLink, DropdownMenuTrigger } from '@app/ui/dropdown';
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

const Form = z.object({
  since: z.date(),
  until: z.date().nullish(),
});

export function EditTenantMembershipForm({ id, onSuccess }: { id: string; onSuccess: () => void }) {
  const { reset, control, handleSubmit } = useZodForm(Form);
  const [query] = useQuery({ query: TenantMembershipDocument, variables: { id }, pause: !id });
  const update = useMutation(UpdateTenantMembershipDocument)[1];

  const item = query.data?.tenantMembership

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
          since: values.since.toISOString(),
          until: values.until ? values.until.toISOString() : null,
        },
      },
    });
    onSuccess?.();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <div className="prose">
        <h4>{item?.person?.name} v klubu {item?.tenant?.name}</h4>
      </div>

      <DatePickerElement control={control} name="since" label="Členství od" />
      <DatePickerElement control={control} name="until" label="Členství do" />

      <div className="flex flex-wrap gap-4">
        <SubmitButton loading={onSubmit.loading}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}

export function EditTenantMembershipCard({ data, showPerson }: { data: TenantMembershipFragment; showPerson?: boolean }) {
  const { perms } = useAuth();
  const [editOpen, setEditOpen] = React.useState(false);
  const update = useMutation(UpdateTenantMembershipDocument)[1];
  const del = useMutation(DeleteTenantMembershipDocument)[1];
  const confirm = useConfirm();

  const endToday = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete členovi ${data.person?.name} ukončit členství ke dnešnímu datu?` })
    await update({ input: { id: data.id, patch: { until: new Date().toISOString() } }});
    toast.success("Členství ukončeno");
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
              <b>Člen klubu {data.tenant?.name}</b>
            )}
            <span>{formatOpenDateRange(data)}</span>
          </div>
        </div>

        <DropdownMenuContent align="start">
          {perms.isAdmin && (
            <DropdownMenuButton onClick={() => setEditOpen(true)}>Upravit členství</DropdownMenuButton>
          )}
          {perms.isAdmin && (
            <DropdownMenuButton onClick={() => endToday()}>Ukončit ke dnešnímu datu</DropdownMenuButton>
          )}
          {perms.isAdmin && (
            <DropdownMenuButton onClick={async () => {
              await confirm({ description: `Opravdu chcete členství NENÁVRATNĚ smazat, včetně všech přiřazených? Spíše použij variantu ukončení členství, ať zůstanou zachována historická data.` })
              await del({ id: data.id });
              toast.success("Odstraněno");
            }}>Smazat</DropdownMenuButton>
          )}
        </DropdownMenuContent>
      </DropdownMenu>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent>
          <EditTenantMembershipForm id={data.id} onSuccess={() => setEditOpen(false)} />
        </DialogContent>
      </Dialog>
    </>
  );
}
