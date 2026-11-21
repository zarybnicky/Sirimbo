import { useAtom } from 'jotai';
import { authAtom, storeRef, useAuth, useTenantId, tenantIdAtom } from '@/lib/auth';
import { normalizeClaims, resolveAuth } from '@/lib/auth-claims';
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
  const tenantId = useTenantId();
  const router = useRouter();
  const [recently] = React.useState(() => Date.now() - 90 * 86_400_000);

  const [candidates] = useQuery({
    query: DevelopmentUsersDocument,
    context: React.useMemo(
      () => ({
        fetchOptions: { headers: { 'x-elevated': '1', 'x-tenant-id': tenantId } },
      }),
      [tenantId],
    ),
  });

  const eligibleUsers = React.useMemo(() => {
    return (candidates.data?.users?.nodes ?? []).flatMap((user) => {
      const persons = user.userProxiesList.flatMap((x) => x.person ?? []);
      const claims = normalizeClaims(user.claims);
      if (!claims || persons.length === 0) return [];
      return {
        id: user.id,
        email: user.uEmail,
        name: persons.map((p) => p.name).join(', '),
        role: resolveAuth(claims, tenantId).role,
        recentlyActive: persons.some(
          (p) =>
            p.lastActivity?.trainingAt &&
            Date.parse(p.lastActivity.trainingAt) >= recently,
        ),
      };
    });
  }, [candidates.data, tenantId, recently]);

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
                  const others = users.filter((x) => x.id !== auth.userId);
                  const available = others.length > 0 ? others : users;
                  const recent = available.filter((x) => x.recentlyActive);
                  const choices = recent.length > 0 ? recent : available;
                  const user = choices[Math.floor(Math.random() * choices.length)]!;
                  login.execute(user.id);
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
                  <DropdownMenuContent align="end" className="z-60">
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
