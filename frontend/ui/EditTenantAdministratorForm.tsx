import { TenantAdministratorDocument, TenantAdministratorFragment, UpdateTenantAdministratorDocument } from '@app/graphql/Memberships';
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

export function EditTenantAdministratorForm({ id, onSuccess }: { id: string; onSuccess: () => void }) {
  const { reset, control, handleSubmit } = useZodForm(Form);
  const [query] = useQuery({ query: TenantAdministratorDocument, variables: { id }, pause: !id });
  const update = useMutation(UpdateTenantAdministratorDocument)[1];

  const item = query.data?.tenantAdministrator

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

      <div className="prose">
        <h4>Administrátor {item?.person?.name} v klubu {item?.tenant?.name}</h4>
      </div>

      <DatePickerElement control={control} name="since" label="Členství od" />
      <DatePickerElement control={control} name="until" label="Členství do" />

      <div className="flex flex-wrap gap-4">
        <SubmitButton loading={onSubmit.loading}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}

export function EditTenantAdministratorCard({ data, showPerson }: { data: TenantAdministratorFragment; showPerson?: boolean; }) {
  const [editOpen, setEditOpen] = React.useState(false);
  const update = useMutation(UpdateTenantAdministratorDocument)[1];
  const confirm = useConfirm();

  const endToday = React.useCallback(async () => {
    await confirm({ description: `Opravdu chcete ${data.person?.name} ukončit správcovství ke dnešnímu datu?` })
    await update({ input: { id: data.id, patch: { until: new Date().toISOString() } }});
    toast.success("Ukončeno");
  }, [update]);

  return (
    <>
      <DropdownMenu key={data.id}>
        <DropdownMenuTrigger asChild>
          <button className={buttonCls({ display: 'listItem', variant: 'outline', className: "flex flex-row justify-between flex-wrap w-full" })}>
            <b>{showPerson ? data.person?.name : `Správce klubu ${data.tenant?.name}`}</b>
            <span>{formatOpenDateRange(data)}</span>
          </button>
        </DropdownMenuTrigger>
        <DropdownMenuContent align="start">
          <DropdownMenuLink href={`/clenove/${data.person?.id}`}>
            Detail člověka
          </DropdownMenuLink>
          <DropdownMenuButton onClick={() => setEditOpen(true)}>Upravit správcovství</DropdownMenuButton>
          <DropdownMenuButton onClick={() => endToday()}>Ukončit ke dnešnímu datu</DropdownMenuButton>
        </DropdownMenuContent>
      </DropdownMenu>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent>
          <EditTenantAdministratorForm id={data.id} onSuccess={() => setEditOpen(false)} />
        </DialogContent>
      </Dialog>
    </>
  );
}