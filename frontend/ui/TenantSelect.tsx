import { useAtom } from 'jotai';
import { storeRef, tenantIdAtom } from '@/ui/state/auth';
import React from 'react';
import { tenantCatalog } from '@/tenant/catalog';
import { useAuth } from '@/ui/use-auth';
import { SelectField } from '@/ui/fields/select';

export function TenantSelect() {
  const auth = useAuth();

  const [tenantId, setTenantId] = useAtom(tenantIdAtom);
  const options = React.useMemo(() => {
    return Object.values(tenantCatalog).map(({ id, name }) => ({
      value: id.toString(),
      label: name,
    }));
  }, []);

  const onChange = React.useCallback(
    (tenantId: string) => {
      setTenantId(tenantId);
      storeRef.resetUrqlClient();
    },
    [setTenantId],
  );

  if (!auth.isSystemAdmin && process.env.NODE_ENV !== 'development') return;

  return (
    <SelectField
      className="text-neutral-12"
      onChange={onChange}
      value={tenantId}
      options={options}
    />
  );
}
