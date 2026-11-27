import { CreateTenantMembershipDocument } from '@/graphql/Memberships';
import { TenantsDocument } from '@/graphql/Tenant';
import { useAuth } from '@/lib/auth';
import { DialogTitle } from '@/ui/dialog';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { DatePickerElement } from '@/ui/fields/date';
import { FormError, useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import { useForm } from 'react-hook-form';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';

const Form = z.object({
  tenantId: z.string().min(1, 'Vyberte klub'),
  since: z.date(),
});

export function AddToOtherTenantForm({ personId }: { personId: string }) {
  const auth = useAuth();
  const { onSuccess } = useFormResult();
  const { control, setError, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues: { tenantId: '', since: new Date() },
  });
  const [tenants] = useQuery({ query: TenantsDocument });
  const [result, create] = useMutation(CreateTenantMembershipDocument);
  const options =
    tenants.data?.tenantsList
      ?.filter(
        (x) => x.id !== auth.currentTenantId && auth.canManageTenant({ tenantId: x.id }),
      )
      .map((x) => ({ id: x.id, label: x.name })) ?? [];

  const onSubmit = async ({ tenantId, since }: z.infer<typeof Form>) => {
    const response = await create({
      input: { tenantMembership: { personId, tenantId, since: since.toISOString() } },
    });
    if (response.error) {
      setError('root.server', { message: response.error.message });
    } else {
      onSuccess();
    }
  };

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit)}>
      <DialogTitle>Přidat do jiného klubu</DialogTitle>
      <FormError error={result.error || tenants.error} />
      <ComboboxElement control={control} name="tenantId" label="Klub" options={options} />
      <DatePickerElement control={control} name="since" label="Datum vstupu do klubu" />
      <SubmitButton
        control={control}
        disabled={tenants.fetching || !!tenants.error || options.length === 0}
      >
        Přidat
      </SubmitButton>
    </form>
  );
}
