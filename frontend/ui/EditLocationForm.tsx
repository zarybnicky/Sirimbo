import { Dialog, DialogContent } from '@app/ui/dialog';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@app/ui/dropdown';
import { useZodForm } from '@/lib/use-schema-form';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { TypeOf, z } from 'zod';
import { FormError } from './form';
import { SubmitButton } from './submit';
import { useAuth } from './use-auth';
import { MoreHorizontal } from 'lucide-react';
import { TenantLocationDocument, TenantLocationFragment, UpdateTenantLocationDocument } from '@/graphql/Tenant';
import { TextField, TextFieldElement } from './fields/text';
import { CheckboxElement } from './fields/checkbox';

const Form = z.object({
  name: z.string(),
  description: z.string(),
  isPublic: z.boolean(),
  address: z.object({
    city: z.string(),
    conscriptionNumber: z.string(),
    district: z.string(),
    orientationNumber: z.string(),
    postalCode: z.string(),
    region: z.string(),
    street: z.string(),
  }),
});

export function EditTenantLocationForm({ id, onSuccess }: { id: string; onSuccess: () => void }) {
  const { reset, control, handleSubmit } = useZodForm(Form);
  const [query] = useQuery({ query: TenantLocationDocument, variables: { id }, pause: !id });
  const update = useMutation(UpdateTenantLocationDocument)[1];

  const item = query.data?.tenantLocation;

  React.useEffect(() => {
    if (item) {
      reset({
        name: item.name,
        description: item.description,
        isPublic: !!item.isPublic,
        address: {
          street: item.address?.street || '',
          conscriptionNumber: item.address?.conscriptionNumber || '',
          orientationNumber: item.address?.orientationNumber || '',
          district: item.address?.district || '',
          city: item.address?.city || '',
          postalCode: item.address?.postalCode || '',
          region: item.address?.region || '',
        },
      });
    }
  }, [reset, item]);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    await update({
      input: {
        id,
        patch: {
          ...values,
          isPublic: !!values.isPublic,
        },
      },
    });
    onSuccess?.();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <TextFieldElement control={control} name="name" label="Jméno" />
      <TextFieldElement control={control} name="description" label="Popis" />

      <TextFieldElement control={control} name="address.street" label="Ulice" />
      <TextFieldElement control={control} name="address.conscriptionNumber" label="Č. popisné" />
      <TextFieldElement control={control} name="address.orientationNumber" label="Č. orientační" />
      <TextFieldElement control={control} name="address.district" label="Část města" />
      <TextFieldElement control={control} name="address.city" label="Město" />
      <TextFieldElement control={control} name="address.postalCode" label="PSČ" />
      <TextFieldElement control={control} name="address.region" label="Kraj" />
      <TextField label="Země" value="Česká republika" disabled />

      <CheckboxElement control={control} name="isPublic" label="Veřejné" />

      <div className="flex flex-wrap gap-4">
        <SubmitButton loading={onSubmit.loading}>Uložit změny</SubmitButton>
      </div>
    </form>
  );
}

export function EditTenantLocationCard({ data }: { data: TenantLocationFragment; }) {
  const { perms } = useAuth();
  const [editOpen, setEditOpen] = React.useState(false);

  return (
    <DropdownMenu key={data.id}>
      <div className="flex gap-3 mb-1 align-center">
        {perms.isAdmin && (
          <DropdownMenuTrigger>
            <MoreHorizontal className="w-5 h-5 text-neutral-10" />
          </DropdownMenuTrigger>
        )}

        <div className="grow gap-2 flex text-sm py-1">
          <b>{data.name}</b>
        </div>
      </div>

      <DropdownMenuContent align="start">
        {perms.isAdmin && (
          <DropdownMenuButton onClick={() => setEditOpen(true)}>Upravit místo</DropdownMenuButton>
        )}
      </DropdownMenuContent>

      <Dialog open={editOpen} onOpenChange={setEditOpen}>
        <DialogContent>
          <EditTenantLocationForm id={data.id} onSuccess={() => setEditOpen(false)} />
        </DialogContent>
      </Dialog>
    </DropdownMenu>
  );
}
