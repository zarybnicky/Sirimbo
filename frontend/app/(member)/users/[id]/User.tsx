'use client';

import { UserDetailDocument } from '@/graphql/CurrentUser';
import { useActionMap } from '@/lib/actions';
import { personActions } from '@/lib/actions/person';
import { ActionRow } from '@/ui/ActionRow';
import { formatOpenDateRange, numericFullFormatter } from '@/ui/format';
import { PageHeader } from '@/ui/TitleBar';
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
  const pageTitle = user?.uEmail ?? '';
  const people = React.useMemo(() => {
    return user?.userProxiesList.flatMap((x) => (x.person ? [x.person] : [])) ?? [];
  }, [user]);

  const personActionMap = useActionMap(personActions, people);

  return (
    <>
      <PageHeader
        title={pageTitle}
        breadcrumbs={[{ label: 'Členové', href: '/clenove' }, { label: pageTitle }]}
      />

      {fetching && <p>Načítám...</p>}
      {error && <p className="text-accent-11">Nepodařilo se načíst uživatele.</p>}
      {!fetching && !error && !user && <p>Uživatel nebyl nalezen.</p>}

      {user && (
        <div className="space-y-6">
          <dl className="tabular text-sm">
            <dt>ID</dt>
            <dd>{user.id}</dd>
            <dt>E-mail</dt>
            <dd>{user.uEmail}</dd>
            <dt>Jméno</dt>
            <dd>{[user.uJmeno, user.uPrijmeni].filter(Boolean).join(' ')}</dd>
            <dt>Vytvořen</dt>
            <dd>{numericFullFormatter.format(new Date(user.createdAt))}</dd>
            <dt>Poslední přihlášení</dt>
            {user.lastLogin && (
              <dd>{numericFullFormatter.format(new Date(user.lastLogin))}</dd>
            )}
            <dt>Naposledy aktivní</dt>
            {user.lastActiveAt && (
              <dd>
                {numericFullFormatter.format(new Date(user.lastActiveAt))}
                {` (${user.lastVersion})`}
              </dd>
            )}
          </dl>

          <section>
            <h2 className="text-lg font-medium">Připojené osoby</h2>
            {user.userProxiesList.map((item) => (
              <ActionRow key={item.id} actions={personActionMap.get(item.personId)!}>
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
