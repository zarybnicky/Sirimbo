'use client';

import {
  type AccessCredentialFragment,
  AccessEventOverviewDocument,
  PeopleAccessOverviewDocument,
} from '@/graphql/AccessCredential';
import type { UserProxyFragment } from '@/graphql/Memberships';
import type { PersonBasicFragment } from '@/graphql/Person';
import { type ResolvedAction, useActionMap, useActions } from '@/lib/actions';
import { accessCredentialActions } from '@/lib/actions/accessCredential';
import { personActions } from '@/lib/actions/person';
import {
  type PersonInvitationActionItem,
  personInvitationActions,
} from '@/lib/actions/personInvitation';
import { userProxyActions } from '@/lib/actions/userProxy';
import { mifareCodeToLabel } from '@/lib/access-credentials';
import { cn } from '@/lib/cn';
import { ActionGroup } from '@/ui/ActionGroup';
import { dateTimeFormatter } from '@/ui/format';
import { AccessCredentialForm } from '@/ui/forms/AccessCredentialForm';
import { TabMenu } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@/ui/dialog';
import {
  Clock3,
  CreditCard,
  KeyRound,
  Mail,
  MailPlus,
  MailX,
  Unplug,
} from 'lucide-react';
import Link from 'next/link';
import { parseAsString, useQueryState } from 'nuqs';
import * as React from 'react';
import { useQuery } from 'urql';

const compactDateTimeFormatter = new Intl.DateTimeFormat('cs-CZ', {
  dateStyle: 'short',
  timeStyle: 'short',
});

function activityClassName(timestamp?: string) {
  if (!timestamp) return 'text-danger-10';
  const days = (Date.now() - Date.parse(timestamp)) / 86_400_000;
  if (days > 90) return 'text-danger-10';
  if (days > 30) return 'text-neutral-11';
  return 'text-green-11';
}

type AccessPerson = PersonBasicFragment & {
  userProxiesList: UserProxyFragment[];
  personInvitationsList: PersonInvitationActionItem[];
};

type ActionMap = ReadonlyMap<string, readonly ResolvedAction[]>;

const noCredentials: readonly AccessCredentialFragment[] = [];

const tabs = [
  { id: 'people', title: 'Osoby', contents: () => <PeopleAccessTab /> },
  { id: 'events', title: 'Přístupy', contents: () => <EventsTab /> },
];

export function AccessCards() {
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );

  return (
    <>
      <PageHeader title="Přístupy" />
      <TabMenu selected={tab} onSelect={setTab} options={tabs} />
    </>
  );
}

function PeopleAccessTab() {
  const [{ data, error }] = useQuery({ query: PeopleAccessOverviewDocument });
  const credentials = React.useMemo(
    () => data?.accessCredentialsList ?? [],
    [data?.accessCredentialsList],
  );
  const people = React.useMemo(
    () => (data?.people?.nodes ?? []).toSorted((a, b) => a.name.localeCompare(b.name)),
    [data?.people?.nodes],
  );
  const credentialsByPerson = React.useMemo(
    () => Map.groupBy(credentials, (x) => x.person?.id),
    [credentials],
  );
  const userProxies = React.useMemo(
    () => people.flatMap((x) => x.userProxiesList),
    [people],
  );
  const invitations = React.useMemo(
    () => people.flatMap((x) => x.personInvitationsList),
    [people],
  );
  const userProxyActionMap = useActionMap(userProxyActions, userProxies);
  const invitationActionMap = useActionMap(personInvitationActions, invitations);
  const credentialActionMap = useActionMap(accessCredentialActions, credentials);

  return (
    <div className="divide-y divide-neutral-5 rounded-md border border-neutral-5">
      {error && <p className="text-danger-11">{error.message}</p>}
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
        <PersonAccessRow
          key={person.id}
          person={person}
          credentials={credentialsByPerson.get(person.id) ?? noCredentials}
          userActionMap={userProxyActionMap}
          invitationActionMap={invitationActionMap}
          credentialActionMap={credentialActionMap}
        />
      ))}
      {people.length === 0 && (
        <p className="px-3 py-2 text-sm text-neutral-11">Žádné osoby.</p>
      )}
    </div>
  );
}

const PersonAccessRow = React.memo(function PersonAccessRow({
  person,
  credentials,
  userActionMap,
  invitationActionMap,
  credentialActionMap,
}: Readonly<{
  person: AccessPerson;
  credentials: readonly AccessCredentialFragment[];
  userActionMap: ActionMap;
  invitationActionMap: ActionMap;
  credentialActionMap: ActionMap;
}>) {
  const userProxies = person.userProxiesList;
  const invitations = person.personInvitationsList;
  const users = userProxies.flatMap(({ status, user }) =>
    status === 'ACTIVE' ? (user ?? []) : [],
  );
  const accountCount = new Set(users.map((x) => x.id)).size;
  const cardCount = credentials.filter((x) => x.isAllowed).length;
  const lastWebActivity = users
    .flatMap((x) => x.lastActiveAt ?? [])
    .toSorted()
    .at(-1);
  const lastCardActivity = credentials
    .flatMap((x) => x.lastUsed ?? [])
    .toSorted()
    .at(-1);
  const hasOnlyExpiredAccounts =
    userProxies.length > 0 && userProxies.every((x) => x.status === 'EXPIRED');

  return (
    <div className="grid grid-cols-[minmax(0,1fr)_auto] gap-x-4 gap-y-2 px-3 py-2 hover:bg-neutral-2 sm:grid-cols-[minmax(10rem,1fr)_10rem_10rem_1.75rem] sm:items-center">
      <Link className="truncate font-semibold underline" href={`/clenove/${person.id}`}>
        {person.name}
      </Link>
      <AccessMenu
        person={{
          ...person,
          canInvite: accountCount === 0 && invitations.length === 0,
        }}
        credentials={credentials}
        userActionMap={userActionMap}
        invitationActionMap={invitationActionMap}
        credentialActionMap={credentialActionMap}
      />
      <div className="col-start-1 row-start-2 flex min-w-0 items-center gap-1 text-xs text-neutral-11 sm:col-start-2 sm:row-start-1">
        <div className="flex min-w-0 items-center gap-1.5" title={`${accountCount} účtů`}>
          <KeyRound className="size-3.5 shrink-0 text-accent-11" aria-hidden="true" />
          <b
            className={cn(
              'tabular-nums',
              accountCount === 0 ? 'text-danger-10' : 'text-neutral-12',
            )}
          >
            {accountCount}
          </b>
          {accountCount > 0 ? (
            <span
              className={cn(
                'ml-1 flex min-w-0 items-center gap-1',
                activityClassName(lastWebActivity),
              )}
            >
              <Clock3 className="size-3 shrink-0" aria-hidden="true" />
              {lastWebActivity ? (
                <time className="truncate leading-tight" dateTime={lastWebActivity}>
                  {compactDateTimeFormatter.format(new Date(lastWebActivity))}
                </time>
              ) : (
                '-'
              )}
            </span>
          ) : invitations.length > 0 ? (
            <span className="ml-1 flex min-w-0 items-center gap-1 text-accent-11">
              <Mail className="size-3 shrink-0" aria-hidden="true" />
              <span className="truncate">Pozvánka odeslána</span>
            </span>
          ) : hasOnlyExpiredAccounts ? (
            <span className="ml-1 flex min-w-0 items-center gap-1 text-danger-10">
              <Unplug className="size-3 shrink-0" aria-hidden="true" />
              <span className="truncate">Přístup skončil</span>
            </span>
          ) : person.email ? (
            <span className="ml-1 flex min-w-0 items-center gap-1 text-neutral-9">
              <MailPlus className="size-3 shrink-0" aria-hidden="true" />
              <span className="truncate">Bez pozvánky</span>
            </span>
          ) : (
            <span className="ml-1 flex min-w-0 items-center gap-1 text-danger-10">
              <MailX className="size-3 shrink-0" aria-hidden="true" />
              <span className="truncate">Chybí e-mail</span>
            </span>
          )}
        </div>
      </div>
      <div className="col-start-2 row-start-2 flex min-w-0 items-center gap-1 text-xs text-neutral-11 sm:col-start-3 sm:row-start-1">
        <div
          className="flex min-w-0 items-center gap-1.5"
          title={`${cardCount} aktivních karet`}
        >
          <CreditCard className="size-3.5 shrink-0 text-accent-11" aria-hidden="true" />
          <b
            className={cn(
              'tabular-nums',
              cardCount === 0 ? 'text-danger-10' : 'text-neutral-12',
            )}
          >
            {cardCount}
          </b>
          <span
            className={cn(
              'ml-1 flex min-w-0 items-center gap-1',
              activityClassName(lastCardActivity),
            )}
          >
            <Clock3 className="ml-1 size-3 shrink-0" aria-hidden="true" />
            {lastCardActivity ? (
              <time className="truncate leading-tight" dateTime={lastCardActivity}>
                {compactDateTimeFormatter.format(new Date(lastCardActivity))}
              </time>
            ) : (
              '-'
            )}
          </span>
        </div>
      </div>
    </div>
  );
});

function EventsTab() {
  const [{ data, error }] = useQuery({ query: AccessEventOverviewDocument });
  const credentials = React.useMemo(
    () => data?.accessCredentialsList ?? [],
    [data?.accessCredentialsList],
  );
  const events = React.useMemo(
    () => data?.accessEventsList ?? [],
    [data?.accessEventsList],
  );
  const allowedCodes = React.useMemo(
    () =>
      new Set(credentials.filter((x) => x.isAllowed).map((x) => `${x.kind}:${x.code}`)),
    [credentials],
  );

  return (
    <div className="divide-y divide-neutral-5 rounded-md border border-neutral-5">
      {error && <p className="text-danger-11">{error.message}</p>}
      {events.map((event) => (
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
          {!event.allowed && !allowedCodes.has(`${event.kind}:${event.code}`) && (
            <Dialog>
              <DialogTrigger size="xs" text="Přidat" />
              <DialogContent>
                <DialogTitle>Přiřadit kartu</DialogTitle>
                <AccessCredentialForm
                  initialValue={{
                    label: mifareCodeToLabel(event.code),
                    code: event.code,
                  }}
                />
              </DialogContent>
            </Dialog>
          )}
        </div>
      ))}
      {events.length === 0 && (
        <p className="px-3 py-2 text-sm text-neutral-11">Žádné události.</p>
      )}
    </div>
  );
}

function AccessMenu({
  person,
  credentials,
  userActionMap,
  invitationActionMap,
  credentialActionMap,
}: Readonly<{
  person: AccessPerson & { canInvite: boolean };
  credentials: readonly AccessCredentialFragment[];
  userActionMap: ActionMap;
  invitationActionMap: ActionMap;
  credentialActionMap: ActionMap;
}>) {
  const resolvedPersonActions = useActions(personActions, person);
  const actions: ResolvedAction[] = [
    ...resolvedPersonActions.filter(({ id }) =>
      ['person.linkUser', 'person.invite'].includes(id),
    ),
    ...person.userProxiesList.flatMap((proxy) =>
      (userActionMap.get(proxy.id) ?? []).filter((x) =>
        ['userProxy.edit', 'userProxy.endToday'].includes(x.id),
      ),
    ),
    ...person.personInvitationsList.flatMap((x) => invitationActionMap.get(x.id) ?? []),
    ...resolvedPersonActions.filter((x) => x.id === 'person.addAccessCredential'),
    ...credentials.flatMap((x) => credentialActionMap.get(x.id) ?? []),
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
