import { useAtom } from 'jotai';
import { storeRef, useAuth, tenantIdAtom } from '@/lib/auth';
import React from 'react';
import { tenantCatalog } from '@/tenant/catalog';
import { SelectField } from '@/ui/fields/select';

export function TenantSelect() {
  const auth = useAuth();

  const [tenantId, setTenantId] = useAtom(tenantIdAtom);
  const options = React.useMemo(() => {
    return Object.values(tenantCatalog)
      .filter((x) => auth.isSystemAdmin || auth.tenantIds.includes(x.id))
      .map(({ id, name }) => ({ value: id.toString(), label: name }));
  }, [auth.isSystemAdmin, auth.tenantIds]);

  const onChange = React.useCallback(
    (tenantId: string) => {
      if (process.env.NODE_ENV === 'production') {
        const { pathname, search, hash } = window.location;
        const params = new URLSearchParams({ tenant: tenantId, from: pathname + search + hash });
        window.location.assign(new URL(`/api/auth/handoff?${params}`, window.location.origin));
        return;
      }
      setTenantId(tenantId);
      storeRef.resetUrqlClient();
    },
    [setTenantId],
  );

  if (!auth.isSystemAdmin && auth.tenantIds.length < 2) return;

  return (
    <SelectField
      className="text-neutral-12"
      onChange={onChange}
      value={tenantId}
      options={options}
    />
  );
}
