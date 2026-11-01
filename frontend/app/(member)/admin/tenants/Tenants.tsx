'use client';

import { DefinitionList } from '@/ui/DefinitionList';
import { RichTextView } from '@/ui/RichTextView';
import { TenantFullFragment, TenantsFullDocument } from '@/graphql/Tenant.ts';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { Table } from '@/ui/Table';
import { cn } from '@/lib/cn';
import { cardCls } from '@/ui/style';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import React from 'react';
import { Sheet, type SheetRef } from 'react-modal-sheet';
import { useQuery } from 'urql';
import { formatAddress } from '@/ui/format.ts';
import { TenantAdminEdit } from '@/ui/forms/TenantAdminForm.tsx';

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
    <div className="col-full-width grid min-w-0 items-start gap-6 lg:grid-cols-[minmax(0,1fr)_minmax(18rem,24rem)]">
      <Table
        columns={['ID', 'Jméno', 'Členové', 'Trenéři', 'Správci']}
        busy={fetching}
        error={error}
      >
        {tenants.map((tenant) => (
          <tr
            key={tenant.id}
            className={cn(
              'cursor-pointer border-t border-neutral-4 hover:bg-neutral-2',
              tenant.id === selectedId && 'bg-neutral-3',
            )}
            onClick={() => setSelectedId(tenant.id)}
          >
            <td>{tenant.id}</td>
            <th scope="row">
              <button
                type="button"
                className="text-left text-accent-11 underline"
                aria-pressed={tenant.id === selectedId}
                onClick={() => setSelectedId(tenant.id)}
              >
                {tenant.name}
              </button>
            </th>
            <td>{tenant.memberships.totalCount}</td>
            <td>{tenant.trainers.totalCount}</td>
            <td>{tenant.administrators.totalCount}</td>
          </tr>
        ))}
      </Table>

      <aside className="hidden min-w-0 lg:sticky lg:top-4 lg:block">
        {selected ? (
          <TenantCard tenant={selected} />
        ) : (
          <div className={cardCls({ className: 'text-sm text-neutral-11' })}>
            Vyber klub ze seznamu.
          </div>
        )}
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
  );
}

function TenantCard({ tenant }: Readonly<{ tenant: TenantFullFragment }>) {
  return (
    <div className={cardCls({ className: 'min-w-0 space-y-3' })}>
      <div className="flex flex-wrap items-center justify-between gap-2">
        <h2 className="text-lg font-bold">{tenant.name}</h2>
        <Dialog>
          <DialogTrigger.Edit size="sm" />
          <DialogContent>
            <TenantAdminEdit tenant={tenant} />
          </DialogContent>
        </Dialog>
      </div>
      <RichTextView className="text-sm text-neutral-12" value={tenant.description} />

      <DefinitionList
        items={{
          ID: tenant.id,
          'Členové': tenant.memberships.totalCount,
          'Trenéři': tenant.trainers.totalCount,
          'Správci': tenant.administrators.totalCount,
          'Bankovní účet': tenant.bankAccount || '-',
          Domény: tenant.origins?.join(', ') ?? '-',
          IČO: tenant.czIco || '-',
          DIČ: tenant.czDic || '-',
          Adresa: formatAddress(tenant.address) || '-',
        }}
      />
      <details className="text-sm">
        <summary className="cursor-pointer font-semibold text-accent-11">
          Nastavení
        </summary>
        <pre className="mt-2 overflow-auto whitespace-pre-wrap wrap-anywhere rounded-sm bg-neutral-3 p-2 text-xs">
          {typeof tenant.tenantSetting?.settings === 'string'
            ? tenant.tenantSetting.settings
            : JSON.stringify(tenant.tenantSetting?.settings ?? {}, null, 2)}
        </pre>
      </details>
    </div>
  );
}
