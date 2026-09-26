'use client';

import { TenantFullFragment, TenantsFullDocument } from '@/graphql/Tenant.ts';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { FormError } from '@/ui/form';
import { Spinner } from '@/ui/Spinner';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import React from 'react';
import { type Column, DataGrid } from 'react-data-grid';
import { Sheet, type SheetRef } from 'react-modal-sheet';
import { useQuery } from 'urql';
import { formatAddress } from '@/ui/format.ts';
import { TenantEditForm } from '@/ui/forms/EditTenantFormFull.tsx';

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

const columns: Column<TenantFullFragment>[] = [
  {
    key: '__actions',
    name: '',
    frozen: true,
    sortable: false,
    resizable: false,
    width: 64,
    renderCell({ row }) {
      return (
        <Dialog>
          <DialogTrigger.Edit text="" size="md" variant="none" />
          <DialogContent>
            <TenantEditForm tenant={row} />
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
  const [{ data, fetching, error }] = useQuery({ query: TenantsFullDocument });
  const tenants = React.useMemo(() => data?.tenantsList ?? [], [data]);

  const [selectedId, setSelectedId] = React.useState<string | null>(null);
  const selected = React.useMemo(
    () => tenants.find((t) => t.id === selectedId) ?? null,
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

      {!fetching && !error && tenants.length === 0 && (
        <div className="mt-6 rounded-md border border-neutral-6 bg-neutral-2 p-6 text-sm text-neutral-11">
          Nebyly nalezeny žádné kluby.
        </div>
      )}

      <FormError error={error} />
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
                  Vyber klub (TODO: cross club statistics, activity?)
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

function TenantCard({ tenant }: Readonly<{ tenant: TenantFullFragment }>) {
  return (
    <div className="space-y-3 rounded-lg border border-neutral-6 bg-neutral-2 p-4">
      <div className="flex items-center justify-between">
        <div className="text-base font-semibold">{tenant.name}</div>
      </div>
      <p className="whitespace-pre-wrap text-sm text-neutral-12">{tenant.description}</p>

      <dl className="text-sm text-neutral-12">
        <dt>Bankovní účet</dt>
        <dd>{tenant.bankAccount || '-'}</dd>
        <dt>Domény</dt>
        <dd>{tenant.origins?.join(', ') ?? '-'}</dd>
        <dt>IČO / DIČ</dt>
        <dd>{[tenant.czIco ?? '-', tenant.czDic].filter(Boolean).join(' / ')}</dd>
        <dt>Adresa</dt>
        <dd>{formatAddress(tenant.address) || '-'}</dd>
      </dl>
    </div>
  );
}
