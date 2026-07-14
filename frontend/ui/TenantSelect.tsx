import { useAtom } from 'jotai';
import { storeRef, tenantIdAtom } from '@/ui/state/auth';
import React from 'react';
import { tenantCatalog } from '@/tenant/catalog';

export function TenantSelect() {
  const [tenantId, setTenantId] = useAtom(tenantIdAtom);
  const onChange = React.useCallback(
    (e: React.ChangeEvent<HTMLSelectElement>) => {
      storeRef.resetUrqlClient();
      setTenantId(e.currentTarget.value);
    },
    [setTenantId],
  );

  return (
    <select className="text-neutral-12" onChange={onChange} value={tenantId}>
      {Object.values(tenantCatalog).map((x) => (
        <option key={x.id} value={x.id}>
          {x.name}
        </option>
      ))}
    </select>
  );
}
