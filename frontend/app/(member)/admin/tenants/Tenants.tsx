'use client';

import { AddressDomain } from '@/graphql';
import {
  AdminTenantsDocument,
  type AdminTenantsQuery,
  ReplaceTenantSettingsDocument,
} from '@/graphql/SystemAdmin';
import { UpdateTenantDocument } from '@/graphql/Tenant';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { TextFieldElement } from '@/ui/fields/text';
import { TextAreaElement } from '@/ui/fields/textarea';
import { FormError, useFormResult } from '@/ui/form';
import { Spinner } from '@/ui/Spinner';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import React from 'react';
import { DataGrid, type Column } from 'react-data-grid';
import { useForm } from 'react-hook-form';
import { Sheet, type SheetRef } from 'react-modal-sheet';
import { useMutation, useQuery } from 'urql';
import z from 'zod';

function useMediaQuery(query: string) {
  const [matches, setMatches] = React.useState(false);

  React.useEffect(() => {
    const m = window.matchMedia(query);
    const onChange = () => setMatches(m.matches);
    onChange();
    m.addEventListener?.('change', onChange);
    return () => m.removeEventListener?.('change', onChange);
  }, [query]);

  return matches;
}

type TenantRow = NonNullable<AdminTenantsQuery['tenantsList']>[number];

const columns: Column<TenantRow>[] = [
  {
    key: '__actions',
    name: '',
    frozen: true,
    sortable: false,
    resizable: false,
    renderCell({ row }) {
      return (
        <Dialog>
          <DialogTrigger.Edit text="" size="md" variant="none" />
          <DialogContent>
            <TenantEditDialog tenant={row} />
          </DialogContent>
        </Dialog>
      );
    },
  },
  { key: 'id', name: 'ID', frozen: true },
  { key: 'name', name: 'Jméno' },
  {
    key: 'memberships',
    name: 'Členové',
    renderCell: ({ row }) => row.memberships.totalCount,
  },
  { key: 'trainers', name: 'Trenéři', renderCell: ({ row }) => row.trainers.totalCount },
  {
    key: 'administrators',
    name: 'Správci',
    renderCell: ({ row }) => row.administrators.totalCount,
  },
];

export function Tenants() {
  const [{ data, fetching, error }] = useQuery({ query: AdminTenantsDocument });
  const tenants = React.useMemo(() => data?.tenantsList ?? [], [data]);

  const [selectedId, setSelectedId] = React.useState<string | null>(null);
  const selected = React.useMemo(
    () => tenants.find((t) => String(t.id) === selectedId) ?? null,
    [tenants, selectedId],
  );

  // Mobile sheet: 2 detents + closed.
  const sheetRef = React.useRef<SheetRef>(null);
  const snapPoints = React.useMemo(() => [0, 0.2, 1], []); // must include 0 and 1 :contentReference[oaicite:1]{index=1}

  useLayoutEffect(() => {
    if (selected) document.querySelector('body')!.style.overscrollBehavior = 'none';

    return () => {
      document.querySelector('body')!.style.overscrollBehavior = 'unset';
    };
  }, [selected]);

  const isDesktop = useMediaQuery('(min-width: 1024px)'); // Tailwind lg

  return (
    <div className="col-full-width flex">
      {fetching && (
        <div className="flex h-40 items-center justify-center">
          <Spinner />
        </div>
      )}

      {error && (
        <div className="rounded-md border border-accent-7 bg-accent-3 p-4 text-sm text-accent-11">
          Nepodařilo se načíst seznam klubů: {error.message}
        </div>
      )}

      {!fetching && !error && tenants.length === 0 && (
        <div className="mt-6 rounded-md border border-neutral-6 bg-neutral-2 p-6 text-sm text-neutral-11">
          Nebyly nalezeny žádné kluby.
        </div>
      )}

      {!fetching && !error && tenants.length > 0 && (
        <div className="mt-6 grid gap-4 lg:grid-cols-[3fr_1fr]">
          <div className="rounded-md border border-neutral-6 bg-neutral-1 overflow-auto">
            <DataGrid
              columns={columns}
              rows={tenants}
              rowKeyGetter={(r) => r.id!}
              defaultColumnOptions={{ resizable: true }}
              headerRowHeight={44}
              rowHeight={44}
              onCellClick={({ row }) => setSelectedId(row.id)}
              rowClass={(row) => (row.id === selectedId ? 'bg-neutral-2' : undefined)}
            />
          </div>

          <aside className="hidden lg:block">
            <div className="sticky top-4 space-y-3">
              {selected ? (
                <TenantCard tenant={selected} />
              ) : (
                <div className="rounded-md border border-neutral-6 bg-neutral-2 p-6 text-sm text-neutral-11">
                  Vyber klub pro detail.
                </div>
              )}
            </div>
          </aside>

          {!isDesktop && (
            <Sheet
              ref={sheetRef}
              isOpen={!!selected}
              onClose={() => setSelectedId(null)}
              snapPoints={snapPoints}
              initialSnap={1}
            >
              <Sheet.Container>
                <Sheet.Header />
                <Sheet.Content
                  scrollStyle={{
                    overscrollBehavior: 'contain', // prevents scroll chaining to body
                    WebkitOverflowScrolling: 'touch', // iOS momentum scroll
                    touchAction: 'pan-y', // let it scroll when enabled
                  }}
                >
                  {selected && <TenantCard tenant={selected} />}
                </Sheet.Content>
              </Sheet.Container>

              {/* Non-blocking: omit Backdrop entirely */}
            </Sheet>
          )}
        </div>
      )}
    </div>
  );
}

const TenantFormSchema = z.object({
  name: z.string().min(1, 'Název je povinný'),
  bankAccount: z.string().optional(),
  origins: z.string().optional(),
  czIco: z.string().optional(),
  czDic: z.string().optional(),
  settings: z.string().refine((value) => {
    try {
      const parsed = JSON.parse(value);
      return parsed !== null && typeof parsed === 'object' && !Array.isArray(parsed);
    } catch {
      return false;
    }
  }, 'Nastavení musí být JSON objekt.'),
});

type TenantFormValues = z.infer<typeof TenantFormSchema>;

type TenantCardProps = {
  tenant: TenantRow;
};

function TenantCard({ tenant }: TenantCardProps) {
  const address = formatAddress(tenant.address);

  return (
    <div className="space-y-3 rounded-lg border border-neutral-6 bg-neutral-2 p-4">
      <div className="flex items-center justify-between">
        <div className="text-base font-semibold">{tenant.name}</div>
      </div>

      <dl className="text-sm text-neutral-12">
        <dt>Bankovní účet</dt>
        <dd>{tenant.bankAccount || '-'}</dd>
        <dt>Domény</dt>
        <dd>{tenant.origins?.join(', ') ?? '-'}</dd>
        <dt>IČO / DIČ</dt>
        <dd>
          {tenant.czIco || '-'}
          {tenant.czDic ? ` / ${tenant.czDic}` : ''}
        </dd>
        <dt>Adresa</dt>
        <dd>{address || '-'}</dd>
      </dl>
    </div>
  );
}

type TenantEditDialogProps = {
  tenant: TenantRow;
};

function TenantEditDialog({ tenant }: TenantEditDialogProps) {
  const defaultValues = React.useMemo(() => createFormState(tenant), [tenant]);
  const { control, handleSubmit, reset } = useForm({
    resolver: zodResolver(TenantFormSchema),
    defaultValues,
  });
  const [result, updateTenant] = useMutation(UpdateTenantDocument);
  const [settingsResult, replaceSettings] = useMutation(ReplaceTenantSettingsDocument);
  const { onSuccess } = useFormResult();

  React.useEffect(() => {
    reset(createFormState(tenant), {
      keepDirtyValues: true,
      keepTouched: true,
      keepErrors: true,
    });
  }, [tenant, reset]);

  const onSubmit = async (values: TenantFormValues) => {
    const updated = await updateTenant({
      input: {
        id: tenant.id,
        patch: {
          name: values.name,
          bankAccount: values.bankAccount ?? '',
          origins: (values.origins ?? '')
            .split(',')
            .map((value) => value.trim())
            .filter(Boolean),
          czIco: values.czIco ?? '',
          czDic: values.czDic ?? '',
        },
      },
    });
    if (updated.error) return;

    const saved = await replaceSettings({
      tenantId: tenant.id,
      settings: values.settings,
    });
    if (!saved.error) onSuccess();
  };

  return (
    <form className="grid gap-4" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error ?? settingsResult.error} />

      <TextFieldElement control={control} name="name" label="Název" required />
      <TextFieldElement control={control} name="bankAccount" label="Bankovní účet" />
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
      <TextAreaElement
        control={control}
        name="settings"
        label="Nastavení (JSON)"
        className="font-mono"
      />

      <SubmitButton control={control}>Uložit změny</SubmitButton>
    </form>
  );
}

function createFormState(tenant: TenantRow): TenantFormValues {
  const settings = tenant.tenantSettingsList[0]?.settings;
  return {
    name: tenant.name || '',
    bankAccount: tenant.bankAccount || '',
    origins: tenant.origins?.join(', ') ?? '',
    czIco: tenant.czIco || '',
    czDic: tenant.czDic || '',
    settings:
      typeof settings === 'string' ? settings : JSON.stringify(settings ?? {}, null, 2),
  };
}

function formatAddress(address?: AddressDomain | null) {
  if (!address) return '';

  const streetParts = [
    address.street,
    [address.conscriptionNumber, address.orientationNumber].filter(Boolean).join('/'),
  ]
    .filter(Boolean)
    .join(' ');

  const cityLine = [address.postalCode, address.city].filter(Boolean).join(' ');

  return [streetParts, address.district, cityLine, address.region]
    .filter(Boolean)
    .join(', ');
}
