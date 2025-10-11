import { Layout } from '@/components/layout/Layout';
import { SubmitButton } from '@/ui/submit';
import { TitleBar } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogFooter, DialogHeader, DialogTitle, DialogTrigger } from '@/ui/dialog';
import { Spinner } from '@/ui/Spinner';
import { typographyCls } from '@/ui/style';
import * as React from 'react';
import { useMutation, useQuery } from 'urql';

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
      pendingMembershipCount
      expiredMembershipCount
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
  pendingMembershipCount: number;
  expiredMembershipCount: number;
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
          Čekající členové: {integerFormatter.format(tenant.pendingMembershipCount)}
        </span>
        <span className="rounded-full bg-neutral-3 px-3 py-1 font-semibold text-neutral-11">
          Expirace: {integerFormatter.format(tenant.expiredMembershipCount)}
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

type TenantFormState = {
  name: string;
  description: string;
  bankAccount: string;
  origins: string;
  czIco: string;
  czDic: string;
};

function TenantEditDialog({ tenant, onUpdated }: TenantEditDialogProps) {
  const [open, setOpen] = React.useState(false);
  const [form, setForm] = React.useState<TenantFormState>(() => createFormState(tenant));
  const [{ fetching, error }, updateTenant] = useMutation<UpdateTenantResult, UpdateTenantVariables>(
    SYSTEM_ADMIN_UPDATE_TENANT_MUTATION
  );

  React.useEffect(() => {
    if (open) {
      setForm(createFormState(tenant));
    }
  }, [open, tenant]);

  const handleSubmit = React.useCallback(async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();

    const origins = form.origins
      .split(',')
      .map((value) => value.trim())
      .filter(Boolean);

    const result = await updateTenant({
      input: {
        tenantId: tenant.id,
        name: form.name,
        description: form.description,
        bankAccount: form.bankAccount,
        origins,
        czIco: form.czIco,
        czDic: form.czDic,
      },
    });

    if (!result.error) {
      onUpdated();
      setOpen(false);
    }
  }, [form, tenant.id, updateTenant, onUpdated]);

  const inputCls = 'w-full rounded-md border border-neutral-6 bg-neutral-1 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-accent-7 focus:border-transparent';

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger.Edit text="Upravit" />
      <DialogContent>
        <DialogHeader>
          <DialogTitle>Upravit {tenant.name}</DialogTitle>
        </DialogHeader>
        <form className="grid gap-4" onSubmit={handleSubmit}>
          <label className="grid gap-1 text-sm">
            <span className="font-semibold text-neutral-11">Název</span>
            <input
              className={inputCls}
              value={form.name}
              onChange={(event) => setForm((prev) => ({ ...prev, name: event.target.value }))}
            />
          </label>
          <label className="grid gap-1 text-sm">
            <span className="font-semibold text-neutral-11">Popis</span>
            <textarea
              className={`${inputCls} min-h-24`}
              value={form.description}
              onChange={(event) => setForm((prev) => ({ ...prev, description: event.target.value }))}
            />
          </label>
          <label className="grid gap-1 text-sm">
            <span className="font-semibold text-neutral-11">Bankovní účet</span>
            <input
              className={inputCls}
              value={form.bankAccount}
              onChange={(event) => setForm((prev) => ({ ...prev, bankAccount: event.target.value }))}
            />
          </label>
          <label className="grid gap-1 text-sm">
            <span className="font-semibold text-neutral-11">Domény (oddělené čárkou)</span>
            <input
              className={inputCls}
              value={form.origins}
              onChange={(event) => setForm((prev) => ({ ...prev, origins: event.target.value }))}
            />
          </label>
          <div className="grid gap-4 sm:grid-cols-2">
            <label className="grid gap-1 text-sm">
              <span className="font-semibold text-neutral-11">IČO</span>
              <input
                className={inputCls}
                value={form.czIco}
                onChange={(event) => setForm((prev) => ({ ...prev, czIco: event.target.value }))}
              />
            </label>
            <label className="grid gap-1 text-sm">
              <span className="font-semibold text-neutral-11">DIČ</span>
              <input
                className={inputCls}
                value={form.czDic}
                onChange={(event) => setForm((prev) => ({ ...prev, czDic: event.target.value }))}
              />
            </label>
          </div>

          {error && (
            <div className="rounded-md border border-red-7 bg-red-3 px-3 py-2 text-sm text-red-11">
              Nepodařilo se uložit změny: {error.message}
            </div>
          )}

          <DialogFooter>
            <SubmitButton loading={fetching}>Uložit změny</SubmitButton>
          </DialogFooter>
        </form>
      </DialogContent>
    </Dialog>
  );
}

function createFormState(tenant: TenantRow): TenantFormState {
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
