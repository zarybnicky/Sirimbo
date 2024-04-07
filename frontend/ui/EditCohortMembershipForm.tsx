import { CohortMembershipDocument, CohortMembershipFragment, DeleteCohortMembershipDocument, UpdateCohortMembershipDocument } from '@/graphql/Memberships';
import { useConfirm } from '@/ui/Confirm';
import { Dialog, DialogContent } from '@/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuLink, DropdownMenuTrigger } from '@/ui/dropdown';
import { DatePickerElement } from '@/ui/fields/date';
import { formatOpenDateRange } from '@/ui/format';
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
import Link from 'next/link';
import { MoreHorizontal } from 'lucide-react';

const Form = z.object({
  since: z.date(),
  until: z.date().nullish(),
});

export function EditCohortMembershipForm({ id, onSuccess }: { id: string; onSuccess: () => void }) {
  const { reset, control, handleSubmit } = useZodForm(Form);
  const [query] = useQuery({ query: CohortMembershipDocument, variables: { id }, pause: !id });
  const update = useMutation(UpdateCohortMembershipDocument)[1];

  const item = query.data?.cohortMembership

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

      <div>{item?.tenant?.name}</div>
      <div><b>{item?.person?.name} ve skupině {item?.cohort?.sName}</b></div>

      <DatePickerElement control={control} name="since" label="Členství od" />
      <DatePickerElement control={control} name="until" label="Členství do" />

      <div className="flex flex-wrap gap-4">
        <SubmitButton loading={onSubmit.loading}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}

export function EditCohortMembershipCard({ data, showPerson }: { data: CohortMembershipFragment; showPerson?: boolean; }) {
  const { perms } = useAuth();
  const [editOpen, setEditOpen] = React.useState(false);
  const update = useMutation(UpdateCohortMembershipDocument)[1];
  const del = useMutation(DeleteCohortMembershipDocument)[1];
  const confirm = useConfirm();

  const endToday = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete členovi ${data.person?.name} ukončit členství ke dnešnímu datu?` })
    await update({ input: { id: data.id, patch: { until: new Date().toISOString() } }});
    toast.success("Členství ukončeno");
  }, [update, confirm, data]);

  return (
    <>
      <DropdownMenu key={data.id}>
        <div className="flex gap-3 mb-1 align-baseline">
          {perms.isAdmin && (
            <DropdownMenuTrigger>
              <MoreHorizontal className="size-5 text-neutral-10" />
            </DropdownMenuTrigger>
          )}

          <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
            {showPerson ? (
              <Link className="underline font-bold" href={`/clenove/${data.person?.id}`}>{data.person?.name}</Link>
            ) : (
              <b>
                Člen skupiny{' '}
                <Link className="underline font-bold" href={`/treninkove-skupiny/${data.cohort?.id}`}>{data.cohort?.sName}</Link>
              </b>
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
          <EditCohortMembershipForm id={data.id} onSuccess={() => setEditOpen(false)} />
        </DialogContent>
      </Dialog>
    </>
  );
}
