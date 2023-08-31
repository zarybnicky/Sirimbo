import { CohortMembershipDocument, CohortMembershipFragment, UpdateCohortMembershipDocument } from '@app/graphql/Memberships';
import { useConfirm } from '@app/ui/Confirm';
import { Dialog, DialogContent } from '@app/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuLink, DropdownMenuTrigger } from '@app/ui/dropdown';
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
import { buttonCls } from './style';

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
          until: values.until ? values.until.toISOString() : undefined,
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
  const [editOpen, setEditOpen] = React.useState(false);
  const update = useMutation(UpdateCohortMembershipDocument)[1];
  const confirm = useConfirm();

  const endToday = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete členovi ${data.person?.name} ukončit členství ke dnešnímu datu?` })
    await update({ input: { id: data.id, patch: { until: new Date().toISOString() } }});
    toast.success("Členství ukončeno");
  }, [update]);

  return (
    <>
      <DropdownMenu key={data.id}>
        <DropdownMenuTrigger asChild>
          <button className={buttonCls({ display: 'listItem', variant: 'outline', className: "flex flex-row justify-between flex-wrap w-full" })}>
            <b>{showPerson ? data.person?.name : `Člen skupiny ${data.cohort?.sName}`}</b>
            <span>{formatOpenDateRange(data)}</span>
          </button>
        </DropdownMenuTrigger>
        <DropdownMenuContent align="start">
          <DropdownMenuLink href={`/clenove/${data.person?.id}`}>
            Detail člověka
          </DropdownMenuLink>
          <DropdownMenuLink href={`/treninkove-skupiny/${data.cohort?.id}`}>
            Detail skupiny
          </DropdownMenuLink>
          <DropdownMenuButton onClick={() => setEditOpen(true)}>Upravit členství</DropdownMenuButton>
          <DropdownMenuButton onClick={() => endToday()}>Ukončit ke dnešnímu datu</DropdownMenuButton>
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
