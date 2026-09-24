'use client';

import {
  AccessCredentialOverviewDocument,
  type AccessCredentialFragment,
  type AccessEventFragment,
} from '@/graphql/AccessCredential';
import type { UserProxyFragment } from '@/graphql/Memberships';
import type { PersonBasicFragment } from '@/graphql/Person';
import { useActionMap, type ResolvedAction } from '@/lib/actions';
import { accessCredentialActions } from '@/lib/actions/accessCredential';
import { personActions } from '@/lib/actions/person';
import { userProxyActions } from '@/lib/actions/userProxy';
import { mifareCodeToLabel } from '@/lib/access-credentials';
import { ActionGroup } from '@/ui/ActionGroup';
import { Combobox } from '@/ui/fields/Combobox';
import { dateTimeFormatter } from '@/ui/format';
import { AccessCredentialForm } from '@/ui/forms/AccessCredentialForm';
import { TabMenu } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@/ui/dialog';
import { Clock3, CreditCard, KeyRound } from 'lucide-react';
import Link from 'next/link';
import { parseAsString, useQueryState } from 'nuqs';
import * as React from 'react';
import { useQuery } from 'urql';

const compactDateTimeFormatter = new Intl.DateTimeFormat('cs-CZ', {
  dateStyle: 'short',
  timeStyle: 'short',
});

type AccessPerson = PersonBasicFragment & {
  label: string;
  userProxies: UserProxyFragment[];
  credentials: AccessCredentialFragment[];
  accountCount: number;
  cardCount: number;
  lastWebActivity?: string;
  lastCardActivity?: string;
};

type ActionMap = ReadonlyMap<string, readonly ResolvedAction[]>;

export function AccessCards() {
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );
  const [{ data, error }] = useQuery({ query: AccessCredentialOverviewDocument });
  const credentials = React.useMemo(
    () => data?.accessCredentialsList ?? [],
    [data?.accessCredentialsList],
  );
  const people = React.useMemo(() => {
    const credentialsByPerson = new Map<string, (typeof credentials)[number][]>();
    for (const credential of credentials) {
      if (!credential.person) continue;
      const items = credentialsByPerson.get(credential.person.id) ?? [];
      items.push(credential);
      credentialsByPerson.set(credential.person.id, items);
    }

    return (data?.people?.nodes ?? [])
      .map((person) => {
        const users = person.userProxiesList.flatMap(({ user }) => user ?? []);
        const personCredentials = credentialsByPerson.get(person.id) ?? [];
        return {
          ...person,
          label: person.name,
          userProxies: person.userProxiesList,
          credentials: personCredentials,
          accountCount: new Set(users.map((x) => x.id)).size,
          cardCount: personCredentials.filter((x) => x.isAllowed).length,
          lastWebActivity: users
            .flatMap((x) => x.lastActiveAt ?? [])
            .toSorted()
            .at(-1),
          lastCardActivity: personCredentials
            .flatMap((x) => x.lastUsed ?? [])
            .toSorted()
            .at(-1),
        };
      })
      .toSorted((a, b) => a.label.localeCompare(b.label));
  }, [credentials, data?.people?.nodes]);
  const events = React.useMemo(
    () => data?.accessEventsList ?? [],
    [data?.accessEventsList],
  );
  const tabs = React.useMemo(
    () => [
      {
        id: 'people',
        title: 'Osoby',
        contents: () => <PeopleTab people={people} credentials={credentials} />,
      },
      {
        id: 'events',
        title: 'Události',
        contents: () => (
          <EventsTab people={people} credentials={credentials} events={events} />
        ),
      },
    ],
    [credentials, events, people],
  );

  return (
    <>
      <PageHeader title="Přístupy" />
      {error && <p className="text-danger-11">{error.message}</p>}
      <TabMenu
        selected={tab}
        onSelect={setTab}
        options={tabs}
      />
    </>
  );
}

const PeopleTab = React.memo(function PeopleTab({
  people,
  credentials,
}: {
  people: AccessPerson[];
  credentials: AccessCredentialFragment[];
}) {
  const userProxies = React.useMemo(
    () => people.flatMap((x) => x.userProxies),
    [people],
  );
  const userProxyActionMap = useActionMap(userProxyActions, userProxies);
  const credentialActionMap = useActionMap(accessCredentialActions, credentials);
  const personActionMap = useActionMap(personActions, people);

  return (
    <div className="divide-y divide-neutral-5 rounded-md border border-neutral-5">
      <div className="hidden grid-cols-[minmax(10rem,1fr)_10rem_10rem_1.75rem] gap-4 bg-neutral-2 px-3 py-2 text-xs font-medium text-neutral-11 sm:grid">
        <span>Osoba</span>
        <span className="flex items-center gap-1.5">
          <KeyRound className="size-3.5" aria-hidden="true" /> Web
        </span>
        <span className="flex items-center gap-1.5">
          <CreditCard className="size-3.5" aria-hidden="true" /> Karta
        </span>
        <span className="sr-only">Akce</span>
      </div>

      {people.map((person) => (
        <div
          key={person.id}
          className="grid grid-cols-[minmax(0,1fr)_auto] gap-x-4 gap-y-2 px-3 py-2 hover:bg-neutral-2 sm:grid-cols-[minmax(10rem,1fr)_10rem_10rem_1.75rem] sm:items-center"
        >
          <Link
            className="truncate font-semibold underline"
            href={`/clenove/${person.id}`}
          >
            {person.label}
          </Link>
          <AccessMenu
            person={person}
            personActionMap={personActionMap}
            userActionMap={userProxyActionMap}
            credentialActionMap={credentialActionMap}
          />
          <div className="col-start-1 row-start-2 flex min-w-0 items-center gap-1 text-xs text-neutral-11 sm:col-start-2 sm:row-start-1">
            <div
              className="flex min-w-0 items-center gap-1.5"
              title={`${person.accountCount} účtů`}
            >
              <KeyRound className="size-3.5 shrink-0 text-accent-11" aria-hidden="true" />
              <b
                className={`tabular-nums ${person.accountCount === 0 ? 'text-danger-10' : 'text-neutral-12'}`}
              >
                {person.accountCount}
              </b>
              <Clock3 className="ml-1 size-3 shrink-0" aria-hidden="true" />
              {person.lastWebActivity ? (
                <time className="truncate leading-tight" dateTime={person.lastWebActivity}>
                  {compactDateTimeFormatter.format(new Date(person.lastWebActivity))}
                </time>
              ) : (
                '-'
              )}
            </div>
          </div>
          <div className="col-start-2 row-start-2 flex min-w-0 items-center gap-1 text-xs text-neutral-11 sm:col-start-3 sm:row-start-1">
            <div
              className="flex min-w-0 items-center gap-1.5"
              title={`${person.cardCount} aktivních karet`}
            >
              <CreditCard className="size-3.5 shrink-0 text-accent-11" aria-hidden="true" />
              <b
                className={`tabular-nums ${person.cardCount === 0 ? 'text-danger-10' : 'text-neutral-12'}`}
              >
                {person.cardCount}
              </b>
              <Clock3 className="ml-1 size-3 shrink-0" aria-hidden="true" />
              {person.lastCardActivity ? (
                <time className="truncate leading-tight" dateTime={person.lastCardActivity}>
                  {compactDateTimeFormatter.format(new Date(person.lastCardActivity))}
                </time>
              ) : (
                '—'
              )}
            </div>
          </div>
        </div>
      ))}
      {people.length === 0 && (
        <p className="px-3 py-2 text-sm text-neutral-11">Žádné osoby.</p>
      )}
    </div>
  );
});

const EventsTab = React.memo(function EventsTab({
  people,
  credentials,
  events,
}: {
  people: AccessPerson[];
  credentials: AccessCredentialFragment[];
  events: AccessEventFragment[];
}) {
  const allowedCodes = React.useMemo(
    () =>
      new Set(
        credentials
          .filter((x) => x.isAllowed)
          .map((x) => `${x.kind}:${x.code}`),
      ),
    [credentials],
  );

  return (
    <div className="divide-y divide-neutral-5 rounded-md border border-neutral-5">
      {events.map((event) => {
        const allowedCard = allowedCodes.has(`${event.kind}:${event.code}`);
        return (
          <div
            key={event.id}
            className="flex flex-wrap items-center justify-between gap-2 px-3 py-2 text-sm"
          >
            <span>
              <b>{event.allowed ? 'Povoleno' : 'Zamítnuto'}</b>
              {' · '}
              {mifareCodeToLabel(event.code)}
              <code className="ml-2 text-neutral-11">{event.code}</code>
              {event.person && (
                <>
                  {' · '}
                  <Link className="underline" href={`/clenove/${event.person.id}`}>
                    {event.person.name}
                  </Link>
                </>
              )}
            </span>
            <span className="ml-auto text-neutral-11">
              {dateTimeFormatter.format(new Date(event.occurredAt))} · {event.device}
              {event.reason && ` · ${event.reason}`}
            </span>
            {!event.allowed && !allowedCard && <AssignCard event={event} people={people} />}
          </div>
        );
      })}
      {events.length === 0 && (
        <p className="px-3 py-2 text-sm text-neutral-11">Žádné události.</p>
      )}
    </div>
  );
});

function AccessMenu({
  person,
  personActionMap,
  userActionMap,
  credentialActionMap,
}: {
  person: AccessPerson;
  personActionMap: ActionMap;
  userActionMap: ActionMap;
  credentialActionMap: ActionMap;
}) {
  const personActions = personActionMap.get(person.id) ?? [];
  const actions: ResolvedAction[] = [
    ...personActions.filter((action) =>
      ['person.linkUser', 'person.invite'].includes(action.id),
    ),
    ...person.userProxies.flatMap((proxy) =>
      (userActionMap.get(proxy.id) ?? []).filter((x) =>
        ['userProxy.edit', 'userProxy.endToday'].includes(x.id),
      ),
    ),
    ...personActions.filter((x) => x.id === 'person.addAccessCredential'),
    ...person.credentials.flatMap((x) => credentialActionMap.get(x.id) ?? []),
  ];

  return (
    <ActionGroup
      className="col-start-2 row-start-1 ml-auto sm:col-start-4"
      actions={actions}
      variant="row"
      align="end"
    />
  );
}

function AssignCard({ event, people }: {
  event: AccessEventFragment;
  people: { id: string; label: string }[];
}) {
  const [personId, setPersonId] = React.useState<string | null>();
  return (
    <Dialog>
      <DialogTrigger size="xs" text="Přidat" />
      <DialogContent>
        <DialogTitle>Přiřadit kartu {mifareCodeToLabel(event.code)}</DialogTitle>
        <Combobox
          value={personId}
          onChange={setPersonId}
          options={people}
          label="Osoba"
          placeholder="Vyberte osobu"
        />
        {personId && (
          <AccessCredentialForm
            key={personId}
            personId={personId}
            initialValue={{
              label: mifareCodeToLabel(event.code),
              code: event.code,
            }}
          />
        )}
      </DialogContent>
    </Dialog>
  );
}
