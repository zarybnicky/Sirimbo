import { CoupleDocument, CoupleFragment, DeleteCoupleDocument, UpdateCoupleDocument } from '@/graphql/Memberships';
import { useConfirm } from '@/ui/Confirm';
import { Dialog, DialogContent } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@/ui/dropdown';
import { DatePickerElement } from '@/ui/fields/date';
import { formatLongCoupleName, formatOpenDateRange } from '@/ui/format';
import { useZodForm } from '@/lib/use-schema-form';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { TypeOf, z } from 'zod';
import { FormError } from './form';
import { SubmitButton } from './submit';
import { useAuth } from './use-auth';
import Link from 'next/link';
import { MoreHorizontal } from 'lucide-react';

const Form = z.object({
  since: z.date().nullish().default(null),
  until: z.date().nullish().default(null),
});

export function EditCoupleForm({ id, onSuccess }: { id: string; onSuccess: () => void }) {
  const { reset, control, handleSubmit } = useZodForm(Form);
  const [query] = useQuery({ query: CoupleDocument, variables: { id }, pause: !id });
  const update = useMutation(UpdateCoupleDocument)[1];

  const item = query.data?.couple

  React.useEffect(() => {
    if (item) {
      reset({
        since: item.since ? new Date(item.since) : null,
        until: item.until ? new Date(item.until) : null,
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
    onSuccess?.();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <div><b>{formatLongCoupleName(item)}</b></div>

      <DatePickerElement control={control} name="since" label="Od" />
      <DatePickerElement control={control} name="until" label="Do" />

      <div className="flex flex-wrap gap-4">
        <SubmitButton loading={onSubmit.loading}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}

export function EditCoupleCard({ data }: { data: CoupleFragment; }) {
  const { perms } = useAuth();
  const [editOpen, setEditOpen] = React.useState(false);
  const update = useMutation(UpdateCoupleDocument)[1];
  const del = useMutation(DeleteCoupleDocument)[1];
  const confirm = useConfirm();

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
            <Link className="underline font-bold" href={`/pary/${data.id}`}>{formatLongCoupleName(data)}</Link>
            <span>{formatOpenDateRange(data)}</span>
          </div>
        </div>

        <DropdownMenuContent align="start">
          {perms.isAdmin && (
            <DropdownMenuButton onClick={() => setEditOpen(true)}>Upravit partnerství</DropdownMenuButton>
          )}
          {perms.isAdmin && (
            <DropdownMenuButton onClick={async () => {
              await confirm({ description: `Opravdu chcete pár ${formatLongCoupleName(data)} ukončit ke dnešnímu datu?` })
              await update({ input: { id: data.id, patch: { until: new Date().toISOString() } }});
              toast.success("Ukončeno");
            }}>Ukončit ke dnešnímu datu</DropdownMenuButton>
          )}
          {perms.isAdmin && (
            <DropdownMenuButton onClick={async () => {
              await confirm({ description: `Opravdu chcete pár NENÁVRATNĚ smazat, včetně všech jejich lekcí, ...? Spíše použij variantu ukončení partnerství, ať zůstanou zachována historická data.` })
              await del({ id: data.id });
              toast.success("Ukončeno");
            }}>Smazat</DropdownMenuButton>
          )}
        </DropdownMenuContent>
      </DropdownMenu>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent>
          <EditCoupleForm id={data.id} onSuccess={() => setEditOpen(false)} />
        </DialogContent>
      </Dialog>
    </>
  );
}
