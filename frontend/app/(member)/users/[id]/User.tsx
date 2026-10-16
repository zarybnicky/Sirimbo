'use client';

import { DefinitionList } from '@/ui/DefinitionList';
import { UserDetailDocument } from '@/graphql/CurrentUser';
import { useActionMap, useActions } from '@/lib/actions';
import { personActions } from '@/lib/actions/person';
import { userProxyActions } from '@/lib/actions/userProxy';
import { userActions } from '@/lib/actions/user';
import { ActionGroup } from '@/ui/ActionGroup';
import { ActionRow } from '@/ui/ActionRow';
import { formatOpenDateRange, numericFullFormatter } from '@/ui/format';
import { PageHeader } from '@/ui/PageHeader';
import Link from 'next/link';
import React from 'react';
import { useQuery } from 'urql';

export function User({ id }: { id: string }) {
  const [{ data, fetching, error }] = useQuery({
    query: UserDetailDocument,
    variables: { id },
    pause: !id,
  });

  const user = data?.user;
  const actions = useActions(userActions, user);
  const pageTitle = user?.uEmail ?? '';
  const people = React.useMemo(() => {
    return user?.userProxiesList.flatMap((x) => (x.person ? [x.person] : [])) ?? [];
  }, [user]);

  const personActionMap = useActionMap(personActions, people);
  const proxyActionMap = useActionMap(userProxyActions, user?.userProxiesList ?? []);

  return (
    <>
      <PageHeader
        title={pageTitle}
        breadcrumbLabel="Uživatel"
        breadcrumbs={[{ label: 'Členové', href: '/clenove' }]}
      />

      {fetching && <p>Načítám...</p>}
      {error && <p className="text-accent-11">Nepodařilo se načíst uživatele.</p>}
      {!fetching && !error && !user && <p>Uživatel nebyl nalezen.</p>}

      {user && (
        <div className="space-y-6">
          <DefinitionList
            items={{
              ID: user.id,
              'E-mail': (
                <a className="underline" href={`mailto:${user.uEmail}`}>
                  {user.uEmail}
                </a>
              ),
              Jméno: [user.uJmeno, user.uPrijmeni].filter(Boolean).join(' '),
              Vytvořen: numericFullFormatter.format(new Date(user.createdAt)),
              'Poslední přihlášení':
                user.lastLogin && numericFullFormatter.format(new Date(user.lastLogin)),
              'Naposledy aktivní': user.lastActiveAt && (
                <>
                  {numericFullFormatter.format(new Date(user.lastActiveAt))}
                  {` (${user.lastVersion})`}
                </>
              ),
            }}
          />

          <section>
            <div className="flex justify-between items-baseline flex-wrap gap-4">
              <h2 className="text-lg font-medium">Připojené osoby</h2>
              <ActionGroup actions={actions} />
            </div>
            {user.userProxiesList.map((item) => (
              <ActionRow
                key={item.id}
                actions={[
                  ...(personActionMap.get(item.personId) ?? []),
                  ...(proxyActionMap.get(item.id) ?? []),
                ]}
              >
                <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
                  <Link
                    className="underline font-bold"
                    href={`/clenove/${item.personId}`}
                  >
                    {item.person?.name}
                  </Link>
                  <span>{formatOpenDateRange(item)}</span>
                </div>
              </ActionRow>
            ))}
          </section>
        </div>
      )}
    </>
  );
}
