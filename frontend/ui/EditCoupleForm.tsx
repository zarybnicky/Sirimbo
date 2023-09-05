import { CoupleDocument, CoupleFragment, UpdateCoupleDocument } from '@app/graphql/Memberships';
import { useConfirm } from '@app/ui/Confirm';
import { Dialog, DialogContent } from '@app/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuLink, DropdownMenuTrigger } from '@app/ui/dropdown';
import { DatePickerElement } from '@app/ui/fields/date';
import { formatLongCoupleName, formatOpenDateRange } from '@app/ui/format';
import { useZodForm } from '@/lib/use-schema-form';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { TypeOf, z } from 'zod';
import { FormError } from './form';
import { SubmitButton } from './submit';
import { buttonCls } from './style';
import { useAuth } from './use-auth';

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
  const confirm = useConfirm();

  const endToday = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete pár ${formatLongCoupleName(data)} ukončit ke dnešnímu datu?` })
    await update({ input: { id: data.id, patch: { until: new Date().toISOString() } }});
    toast.success("Ukončeno");
  }, [update]);

  return (
    <>
      <DropdownMenu key={data.id}>
        <DropdownMenuTrigger asChild>
          <button className={buttonCls({ display: 'listItem', variant: 'outline', className: "flex flex-row justify-between flex-wrap w-full" })}>
              <b>{formatLongCoupleName(data)}</b>
            <span>{formatOpenDateRange(data)}</span>
          </button>
        </DropdownMenuTrigger>
        <DropdownMenuContent align="start">
          <DropdownMenuLink href={`/pary/${data.id}`}>
            Detail páru
          </DropdownMenuLink>
          <DropdownMenuLink href={`/clenove/${data.man?.id}`}>
            Detail partnera
          </DropdownMenuLink>
          <DropdownMenuLink href={`/clenove/${data.woman?.id}`}>
            Detail partnerky
          </DropdownMenuLink>
          {perms.isAdmin && (
            <DropdownMenuButton onClick={() => setEditOpen(true)}>Upravit partnerství</DropdownMenuButton>
          )}
          {perms.isAdmin && (
            <DropdownMenuButton onClick={() => endToday()}>Ukončit ke dnešnímu datu</DropdownMenuButton>
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
