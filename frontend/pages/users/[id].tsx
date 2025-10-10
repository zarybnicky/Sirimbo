import React from 'react';
import { Layout } from '@/components/layout/Layout';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { z } from 'zod';
import { useQuery } from 'urql';
import Link from 'next/link';
import { CornerLeftUp } from 'lucide-react';
import { fullDateFormatter } from '@/ui/format';
import type { UserDetailQuery, UserDetailQueryVariables } from '@/graphql/CurrentUser';
import { UserDetailDocument } from '@/graphql/CurrentUser';

const QueryParams = z.object({
  id: zRouterId,
});

function formatDate(value?: string | null) {
  if (!value) return '—';
  try {
    return fullDateFormatter.format(new Date(value));
  } catch {
    return value;
  }
}

function UserPage() {
  const router = useTypedRouter(QueryParams);
  const { id } = router.query;
  const [{ data, fetching, error }] = useQuery<UserDetailQuery, UserDetailQueryVariables>({
    query: UserDetailDocument,
    variables: { id },
    pause: !id,
  });

  const user = data?.user;

  return (
    <Layout requireAdmin>
      <div className="mx-auto w-full max-w-3xl space-y-6">
        <div className="lg:hidden">
          <Link href="/clenove" className="flex gap-2">
            <CornerLeftUp className="size-4" />
            Zpět na seznam členů
          </Link>
        </div>

        <header className="space-y-1">
          <h1 className="text-2xl font-semibold">Uživatel {user?.uEmail ?? id}</h1>
          <p className="text-neutral-10 text-sm">
            Login: {user?.uLogin ?? '—'}
          </p>
        </header>

        {fetching && <p>Načítám...</p>}
        {error && <p className="text-accent-11">Nepodařilo se načíst uživatele.</p>}
        {!fetching && !error && !user && <p>Uživatel nebyl nalezen.</p>}

        {user && (
          <div className="space-y-6">
            <section>
              <h2 className="text-lg font-medium">Základní informace</h2>
              <dl className="mt-2 grid gap-x-6 gap-y-2 sm:grid-cols-2">
                <div>
                  <dt className="text-sm text-neutral-10">ID</dt>
                  <dd className="text-sm font-medium">{user.id}</dd>
                </div>
                <div>
                  <dt className="text-sm text-neutral-10">E-mail</dt>
                  <dd className="text-sm font-medium">{user.uEmail}</dd>
                </div>
                <div>
                  <dt className="text-sm text-neutral-10">Jméno</dt>
                  <dd className="text-sm font-medium">
                    {[user.uJmeno, user.uPrijmeni].filter(Boolean).join(' ') || '—'}
                  </dd>
                </div>
                <div>
                  <dt className="text-sm text-neutral-10">Tenant ID</dt>
                  <dd className="text-sm font-medium">{user.tenantId}</dd>
                </div>
                <div>
                  <dt className="text-sm text-neutral-10">Vytvořen</dt>
                  <dd className="text-sm font-medium">{formatDate(user.uCreatedAt)}</dd>
                </div>
                <div>
                  <dt className="text-sm text-neutral-10">Poslední přihlášení</dt>
                  <dd className="text-sm font-medium">{formatDate(user.lastLogin)}</dd>
                </div>
                <div>
                  <dt className="text-sm text-neutral-10">Poslední aktivita</dt>
                  <dd className="text-sm font-medium">{formatDate(user.lastActiveAt)}</dd>
                </div>
                <div>
                  <dt className="text-sm text-neutral-10">Poslední verze aplikace</dt>
                  <dd className="text-sm font-medium">{user.lastVersion ?? '—'}</dd>
                </div>
              </dl>
            </section>

            <section>
              <h2 className="text-lg font-medium">Propojené osoby</h2>
              {user.userProxiesList.length === 0 ? (
                <p className="text-sm text-neutral-10">Žádné propojené osoby.</p>
              ) : (
                <ul className="space-y-3">
                  {user.userProxiesList.map((proxy) => (
                    <li key={proxy.id} className="rounded-md border p-3">
                      <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between">
                        <div>
                          <p className="font-medium">
                            {proxy.person ? (
                              <Link
                                href={{ pathname: '/clenove/[id]', query: { id: proxy.person.id } }}
                                className="underline"
                              >
                                {proxy.person.name}
                              </Link>
                            ) : (
                              '—'
                            )}
                          </p>
                          <p className="text-xs text-neutral-10">Status: {proxy.status.toLowerCase()}</p>
                        </div>
                        <div className="text-xs text-neutral-10 mt-2 sm:mt-0">
                          {proxy.active ? 'Aktivní' : 'Neaktivní'}
                        </div>
                      </div>
                      <div className="mt-2 text-xs text-neutral-10 space-y-1">
                        <div>Od: {formatDate(proxy.since)}</div>
                        <div>Do: {formatDate(proxy.until)}</div>
                      </div>
                    </li>
                  ))}
                </ul>
              )}
            </section>
          </div>
        )}
      </div>
    </Layout>
  );
}

export default UserPage;
