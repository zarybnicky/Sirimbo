import { useAtom, useAtomValue } from 'jotai';
import { authAtom, storeRef, useAuth, tenantAtom, tenantIdAtom } from '@/lib/auth';
import { getDevelopmentUsers, pickDevelopmentUser } from '@/lib/development-identity';
import { logInAsAction } from '@/lib/auth-actions';
import { DevelopmentUsersDocument } from '@/graphql/CurrentUser';
import React from 'react';
import { tenantCatalog } from '@/tenant/catalog';
import { SelectField } from '@/ui/fields/select';
import { useAsyncCallback } from 'react-async-hook';
import { useQuery } from 'urql';
import { useRouter } from 'next/navigation';
import { ChevronDown } from 'lucide-react';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { buttonCls, buttonGroupCls } from '@/ui/style';
import { FormError } from '@/ui/form';

const roles = [
  { label: 'Non-member', role: 'anonymous' },
  { label: 'Member', role: 'member' },
  { label: 'Trainer', role: 'trainer' },
  { label: 'Admin', role: 'administrator' },
  { label: 'System admin', role: 'system_admin' },
];

export function TenantSelect() {
  const auth = useAuth();
  const [tenantId, setTenantId] = useAtom(tenantIdAtom);
  const isElevated = process.env.NODE_ENV === 'development' || auth.isSystemAdmin;

  const options = React.useMemo(() => {
    return Object.values(tenantCatalog)
      .filter((x) => isElevated || auth.tenantIds.includes(x.id))
      .map(({ id, name }) => ({ value: id.toString(), label: name }));
  }, [isElevated, auth.tenantIds]);

  const onChange = React.useCallback(
    (tenantId: string) => {
      if (process.env.NODE_ENV === 'production') {
        const { pathname, search, hash } = window.location;
        const params = new URLSearchParams({
          tenant: tenantId,
          from: pathname + search + hash,
        });
        window.location.assign(
          new URL(`/api/auth/handoff?${params}`, window.location.origin),
        );
      } else {
        setTenantId(tenantId);
        storeRef.resetUrqlClient();
      }
    },
    [setTenantId],
  );

  return (
    <>
      {(isElevated || auth.tenantIds.length > 1) && (
        <SelectField
          className="text-neutral-12"
          onChange={onChange}
          value={tenantId}
          options={options}
        />
      )}
      {isElevated && <DevelopmentIdentity />}
    </>
  );
}

function DevelopmentIdentity() {
  const auth = useAuth();
  const tenant = useAtomValue(tenantAtom);
  const tenantId = String(tenant.id);
  const router = useRouter();
  const [now] = React.useState(Date.now);

  const [candidates] = useQuery({
    query: DevelopmentUsersDocument,
    context: React.useMemo(
      () => ({
        fetchOptions: { headers: { 'x-elevated': '1', 'x-tenant-id': tenantId } },
      }),
      [tenantId],
    ),
  });

  const candidateUsers = candidates.data?.users?.nodes;
  const eligibleUsers = React.useMemo(
    () => getDevelopmentUsers(candidateUsers ?? [], tenant, now),
    [candidateUsers, tenant, now],
  );

  const login = useAsyncCallback(async (id: string) => {
    const result = await logInAsAction(id);
    if ('error' in result) throw new Error(result.error);
    storeRef.current.set(authAtom, result.auth);
    storeRef.resetUrqlClient();
    router.refresh();
  });

  return (
    <details className="group">
      <summary
        className={buttonCls({
          variant: 'outline',
          className: 'w-full cursor-pointer justify-between',
        })}
      >
        Přihlásit jako
        <ChevronDown className="transition-transform group-open:rotate-180" />
      </summary>
      <div className="grid gap-1 pt-1">
        {roles.map(({ label, role }) => {
          const users = eligibleUsers.filter((u) => u.role === role);
          return (
            <div key={role} className={buttonGroupCls()}>
              <button
                type="button"
                disabled={users.length === 0}
                aria-pressed={auth.role === role}
                className={buttonCls({ variant: 'outline', className: 'grow' })}
                onClick={() => {
                  const user = pickDevelopmentUser(users, auth.userId);
                  if (user) login.execute(user.id);
                }}
              >
                {label}
              </button>
              {users.length > 1 && (
                <DropdownMenu>
                  <DropdownMenuTrigger asChild>
                    <button type="button" className={buttonCls({ variant: 'outline' })}>
                      <ChevronDown />
                    </button>
                  </DropdownMenuTrigger>
                  <DropdownMenuContent align="end">
                    {users.map((user) => (
                      <DropdownMenuButton key={user.id} onSelect={() => login.execute(user.id)}>
                        <span>
                          {user.name}
                          <span className="block text-xs opacity-70">{user.email}</span>
                        </span>
                      </DropdownMenuButton>
                    ))}
                  </DropdownMenuContent>
                </DropdownMenu>
              )}
            </div>
          );
        })}
        <FormError error={candidates.error || login.error} />
      </div>
    </details>
  );
}
