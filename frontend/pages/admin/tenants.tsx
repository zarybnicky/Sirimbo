import { Layout } from '@/components/layout/Layout';
import { useZodForm } from '@/lib/use-schema-form';
import { TextFieldElement } from '@/ui/fields/text';
import { TextAreaElement } from '@/ui/fields/textarea';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { TitleBar } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogFooter, DialogHeader, DialogTitle, DialogTrigger } from '@/ui/dialog';
import { Spinner } from '@/ui/Spinner';
import { typographyCls } from '@/ui/style';
import * as React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';

const integerFormatter = new Intl.NumberFormat('cs-CZ');
const decimalFormatter = new Intl.NumberFormat('cs-CZ', { maximumFractionDigits: 1 });

const SYSTEM_ADMIN_TENANTS_QUERY = /* GraphQL */ `
  query SystemAdminTenants {
    systemAdminTenants {
      id
      name
      description
      bankAccount
      origins
      czIco
      czDic
      address {
        street
        conscriptionNumber
        orientationNumber
        district
        city
        region
        postalCode
      }
      membershipCount
      trainerCount
      administratorCount
      sessionCountLast30Days
      sessionCountPerTrainerLast30Days
    }
  }
`;

const SYSTEM_ADMIN_UPDATE_TENANT_MUTATION = /* GraphQL */ `
  mutation SystemAdminUpdateTenant($input: SystemAdminUpdateTenantInput!) {
    systemAdminUpdateTenant(input: $input) {
      tenant {
        id
        name
        description
        bankAccount
        origins
        czIco
        czDic
        address {
          street
          conscriptionNumber
          orientationNumber
          district
          city
          region
          postalCode
        }
      }
    }
  }
`;

type AddressDomain = {
  street?: string | null;
  conscriptionNumber?: string | null;
  orientationNumber?: string | null;
  district?: string | null;
  city?: string | null;
  region?: string | null;
  postalCode?: string | null;
};

type TenantRow = {
  id: string;
  name: string;
  description: string;
  bankAccount: string;
  origins: string[] | null;
  czIco: string;
  czDic: string;
  address: AddressDomain | null;
  membershipCount: number;
  trainerCount: number;
  administratorCount: number;
  sessionCountLast30Days: number;
  sessionCountPerTrainerLast30Days: number;
};

type SystemAdminTenantsQueryResult = {
  systemAdminTenants: TenantRow[];
};

type UpdateTenantVariables = {
  input: {
    tenantId: string;
    name: string;
    description: string;
    bankAccount: string;
    origins: string[];
    czIco: string;
    czDic: string;
  };
};

type UpdateTenantResult = {
  systemAdminUpdateTenant: {
    tenant: {
      id: string;
      name: string;
      description: string;
      bankAccount: string;
      origins: string[] | null;
      czIco: string;
      czDic: string;
      address: AddressDomain | null;
    };
  };
};

const TenantFormSchema = z.object({
  name: z.string().min(1, 'Název je povinný'),
  description: z.string().optional(),
  bankAccount: z.string().optional(),
  origins: z.string().optional(),
  czIco: z.string().optional(),
  czDic: z.string().optional(),
});

type TenantFormValues = z.infer<typeof TenantFormSchema>;

export default function SystemAdminTenantsPage() {
  const [{ data, fetching, error }, reexecute] = useQuery<SystemAdminTenantsQueryResult>({
    query: SYSTEM_ADMIN_TENANTS_QUERY,
  });

  return (
    <Layout requireSystemAdmin>
      <TitleBar title="Správa tenantů" />

      {fetching && (
        <div className="flex h-40 items-center justify-center">
          <Spinner />
        </div>
      )}

      {error && (
        <div className="rounded-md border border-red-7 bg-red-3 p-4 text-sm text-red-11">
          Nepodařilo se načíst seznam tenantů: {error.message}
        </div>
      )}

      <div className="mt-6 grid gap-4">
        {data?.systemAdminTenants?.map((tenant) => (
          <TenantCard key={tenant.id} tenant={tenant} onUpdated={() => reexecute({ requestPolicy: 'network-only' })} />
        ))}
        {!fetching && !error && !data?.systemAdminTenants?.length && (
          <div className="rounded-md border border-neutral-6 bg-neutral-2 p-6 text-sm text-neutral-11">
            Nebyly nalezeny žádné tenanty.
          </div>
        )}
      </div>
    </Layout>
  );
}

type TenantCardProps = {
  tenant: TenantRow;
  onUpdated: () => void;
};

function TenantCard({ tenant, onUpdated }: TenantCardProps) {
  const address = formatAddress(tenant.address);

  return (
    <div className="space-y-3 rounded-lg border border-neutral-6 bg-neutral-2 p-4">
      <div className="flex flex-wrap items-start justify-between gap-3">
        <div>
          <div className={typographyCls({ variant: 'section', className: 'mb-1' })}>{tenant.name}</div>
          <div className="text-xs text-neutral-11">ID: {tenant.id}</div>
        </div>
        <TenantEditDialog tenant={tenant} onUpdated={onUpdated} />
      </div>

      {tenant.description && (
        <p className="whitespace-pre-wrap text-sm text-neutral-12">{tenant.description}</p>
      )}

      <dl className="grid gap-2 text-sm text-neutral-12 sm:grid-cols-2">
        <div>
          <dt className="font-semibold text-neutral-11">Bankovní účet</dt>
          <dd>{tenant.bankAccount || '—'}</dd>
        </div>
        <div>
          <dt className="font-semibold text-neutral-11">Domény</dt>
          <dd>{tenant.origins?.length ? tenant.origins.join(', ') : '—'}</dd>
        </div>
        <div>
          <dt className="font-semibold text-neutral-11">IČO / DIČ</dt>
          <dd>
            {tenant.czIco || '—'}
            {tenant.czDic ? ` / ${tenant.czDic}` : ''}
          </dd>
        </div>
        <div>
          <dt className="font-semibold text-neutral-11">Adresa</dt>
          <dd>{address || '—'}</dd>
        </div>
      </dl>

      <div className="flex flex-wrap gap-3 text-xs">
        <span className="rounded-full bg-neutral-3 px-3 py-1 font-semibold text-neutral-11">
          Aktivní členové: {integerFormatter.format(tenant.membershipCount)}
        </span>
        <span className="rounded-full bg-neutral-3 px-3 py-1 font-semibold text-neutral-11">
          Lekce (30 dní): {integerFormatter.format(tenant.sessionCountLast30Days)}
        </span>
        <span className="rounded-full bg-neutral-3 px-3 py-1 font-semibold text-neutral-11">
          Lekce / trenér (30 dní): {decimalFormatter.format(tenant.sessionCountPerTrainerLast30Days)}
        </span>
        <span className="rounded-full bg-neutral-3 px-3 py-1 font-semibold text-neutral-11">
          Trenéři: {integerFormatter.format(tenant.trainerCount)}
        </span>
        <span className="rounded-full bg-neutral-3 px-3 py-1 font-semibold text-neutral-11">
          Administrátoři: {integerFormatter.format(tenant.administratorCount)}
        </span>
      </div>
    </div>
  );
}

type TenantEditDialogProps = {
  tenant: TenantRow;
  onUpdated: () => void;
};

function TenantEditDialog({ tenant, onUpdated }: TenantEditDialogProps) {
  const [open, setOpen] = React.useState(false);
  const defaultValues = React.useMemo(() => createFormState(tenant), [tenant]);
  const { control, handleSubmit, reset } = useZodForm(TenantFormSchema, { defaultValues });
  const [, updateTenant] = useMutation<UpdateTenantResult, UpdateTenantVariables>(
    SYSTEM_ADMIN_UPDATE_TENANT_MUTATION
  );

  React.useEffect(() => {
    if (open) {
      reset(createFormState(tenant));
    }
  }, [open, tenant, reset]);

  const onSubmit = useAsyncCallback(async (values: TenantFormValues) => {
    const origins = (values.origins ?? '')
      .split(',')
      .map((value) => value.trim())
      .filter(Boolean);

    const result = await updateTenant({
      input: {
        tenantId: tenant.id,
        name: values.name,
        description: values.description ?? '',
        bankAccount: values.bankAccount ?? '',
        origins,
        czIco: values.czIco ?? '',
        czDic: values.czDic ?? '',
      },
    });

    if (result.error) {
      throw result.error;
    }

    onUpdated();
    setOpen(false);
  });

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger.Edit text="Upravit" />
      <DialogContent>
        <DialogHeader>
          <DialogTitle>Upravit {tenant.name}</DialogTitle>
        </DialogHeader>
        <form className="grid gap-4" onSubmit={handleSubmit(onSubmit.execute)}>
          <FormError error={onSubmit.error ?? null} />

          <TextFieldElement
            control={control}
            name="name"
            label="Název"
            required
          />
          <TextAreaElement
            control={control}
            name="description"
            label="Popis"
            className="min-h-24"
          />
          <TextFieldElement
            control={control}
            name="bankAccount"
            label="Bankovní účet"
          />
          <TextFieldElement
            control={control}
            name="origins"
            label="Domény (oddělené čárkou)"
            helperText="např. example.cz, www.example.cz"
          />
          <div className="grid gap-4 sm:grid-cols-2">
            <TextFieldElement control={control} name="czIco" label="IČO" />
            <TextFieldElement control={control} name="czDic" label="DIČ" />
          </div>

          <DialogFooter>
            <SubmitButton loading={onSubmit.loading}>Uložit změny</SubmitButton>
          </DialogFooter>
        </form>
      </DialogContent>
    </Dialog>
  );
}

function createFormState(tenant: TenantRow): TenantFormValues {
  return {
    name: tenant.name,
    description: tenant.description || '',
    bankAccount: tenant.bankAccount || '',
    origins: tenant.origins?.join(', ') ?? '',
    czIco: tenant.czIco || '',
    czDic: tenant.czDic || '',
  };
}

function formatAddress(address?: AddressDomain | null) {
  if (!address) return '';

  const streetParts = [
    address.street,
    [address.conscriptionNumber, address.orientationNumber].filter(Boolean).join('/'),
  ].filter(Boolean).join(' ');

  const cityLine = [address.postalCode, address.city].filter(Boolean).join(' ');

  return [streetParts, address.district, cityLine, address.region].filter(Boolean).join(', ');
}
