import { UserProxyDocument, UserProxyFragment, UpdateUserProxyDocument, DeleteUserProxyDocument } from '@app/graphql/Memberships';
import { useConfirm } from '@app/ui/Confirm';
import { Dialog, DialogContent } from '@app/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@app/ui/dropdown';
import { DatePickerElement } from '@app/ui/fields/date';
import { formatOpenDateRange } from '@app/ui/format';
import { useZodForm } from '@/lib/use-schema-form';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { TypeOf, z } from 'zod';
import { FormError } from './form';
import { SubmitButton } from './submit';
import { useAuth } from './use-auth';
import { MoreHorizontal } from 'lucide-react';

const Form = z.object({
  since: z.date(),
  until: z.date().nullish(),
});

export function EditUserProxyForm({ id, onSuccess }: { id: string; onSuccess: () => void }) {
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

export function EditUserProxyCard({ data }: { data: UserProxyFragment; }) {
  const { perms } = useAuth();
  const [editOpen, setEditOpen] = React.useState(false);
  const update = useMutation(UpdateUserProxyDocument)[1];
  const del = useMutation(DeleteUserProxyDocument)[1];
  const confirm = useConfirm();

  const endToday = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete ukončit platnost těchto přihlašovacích údajů?` })
    await update({ input: { id: data.id, patch: { until: new Date().toISOString() } }});
    toast.success("Ukončeno");
  }, [confirm, data.id, update]);

  return (
    <DropdownMenu key={data.id}>
      <div className="flex gap-3 mb-1 align-baseline">
        {perms.isAdmin && (
          <DropdownMenuTrigger>
            <MoreHorizontal className="w-5 h-5 text-neutral-10" />
          </DropdownMenuTrigger>
        )}

        <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
          <b>{data.user?.uEmail}, {data.user?.uLogin}</b>
          <span>{formatOpenDateRange(data)}</span>
        </div>
      </div>

      <DropdownMenuContent align="start">
        {perms.isAdmin && (
          <DropdownMenuButton onClick={() => setEditOpen(true)}>Upravit platnost</DropdownMenuButton>
        )}
        {perms.isAdmin && (
          <DropdownMenuButton onClick={() => endToday()}>Ukončit ke dnešnímu datu</DropdownMenuButton>
        )}
        {perms.isAdmin && (
          <DropdownMenuButton onClick={async () => {
            await confirm({ description: `Opravdu chcete přístupové údaje NENÁVRATNĚ smazat, včetně všech přiřazených dat?` })
            await del({ id: data.id });
            toast.success("Ukončeno");
          }}>Smazat</DropdownMenuButton>
        )}
      </DropdownMenuContent>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent>
          <EditUserProxyForm id={data.id} onSuccess={() => setEditOpen(false)} />
        </DialogContent>
      </Dialog>
    </DropdownMenu>
  );
}
