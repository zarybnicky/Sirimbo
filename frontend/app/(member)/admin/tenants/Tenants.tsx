'use client';

import { DefinitionList } from '@/ui/DefinitionList';
import { RichTextView } from '@/ui/RichTextView';
import { formatAddress } from '@/ui/format';
import { TenantFragment, TenantsDocument } from '@/graphql/Tenant.ts';
import { useActions } from '@/lib/actions';
import { tenantActions } from '@/lib/actions/tenant';
import { ActionGroup } from '@/ui/ActionGroup';
import { Table } from '@/ui/Table';
import { cardCls } from '@/ui/style';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import React from 'react';
import { Sheet } from 'react-modal-sheet';
import { useQuery } from 'urql';
import { parseAsString, useQueryState } from 'nuqs';

// Mobile sheet: 2 detents + closed.
const snapPoints = [0, 0.2, 1];

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
  const [{ data, fetching, error }] = useQuery({ query: TenantsDocument });
  const tenants = data?.tenantsList;

  const [selectedId, setSelectedId] = useQueryState(
    'tenant',
    parseAsString.withOptions({ history: 'push' }),
  );
  const selected = tenants?.find((t) => t.id === selectedId) ?? null;

  useLayoutEffect(() => {
    if (selected) document.body.style.overscrollBehavior = 'none';
    return () => {
      document.body.style.overscrollBehavior = 'unset';
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
        {tenants?.map((tenant) => (
          <tr
            key={tenant.id}
            className={tenant.id === selectedId ? 'bg-neutral-3' : undefined}
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

function TenantCard({ tenant }: Readonly<{ tenant: TenantFragment }>) {
  const actions = useActions(tenantActions, tenant);
  return (
    <div className={cardCls({ className: 'min-w-0 space-y-3' })}>
      <div className="flex flex-wrap items-center justify-between gap-2">
        <h2 className="text-lg font-bold">{tenant.name}</h2>
        <ActionGroup actions={actions} />
      </div>
      <RichTextView className="text-sm text-neutral-12" value={tenant.description} />
      <DefinitionList
        items={{
          ID: tenant.id,
          Členové: tenant.memberships.totalCount,
          Trenéři: tenant.trainers.totalCount,
          Správci: tenant.administrators.totalCount,
          'Bankovní účet': tenant.bankAccount || '-',
          'ČSTS ID': tenant.cstsId,
          Domény: tenant.origins.join(', ') || '-',
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
          {JSON.stringify(tenant.tenantSetting?.settings ?? {}, null, 2)}
        </pre>
      </details>
    </div>
  );
}
