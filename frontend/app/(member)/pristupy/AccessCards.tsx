'use client';

import {
  type AccessCredentialFragment,
  AccessEventOverviewDocument,
  PeopleAccessOverviewDocument,
} from '@/graphql/AccessCredential';
import { CreateInvitationDocument } from '@/graphql/Invitation';
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
import { useTenantConfig } from '@/lib/auth';
import { cn } from '@/lib/cn';
import { ActionGroup } from '@/ui/ActionGroup';
import { dateTimeFormatter } from '@/ui/format';
import { AccessCredentialForm } from '@/ui/forms/AccessCredentialForm';
import { TabMenu } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@/ui/dialog';
import { SubmitButton } from '@/ui/submit';
import { Clock3, CreditCard, KeyRound } from 'lucide-react';
import Link from 'next/link';
import { parseAsString, useQueryState } from 'nuqs';
import * as React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';

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

const normalizeEmail = (email?: string | null) => email?.trim().toLowerCase() ?? '';

function peopleCount(count: number) {
  return count === 1 ? '1 osoba' : count < 5 ? `${count} osoby` : `${count} osob`;
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
  const { enableStarletImport } = useTenantConfig();
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );

  return (
    <>
      <PageHeader title="Přístupy" />
      {enableStarletImport ? (
        <TabMenu selected={tab} onSelect={setTab} options={tabs} />
      ) : (
        <PeopleAccessTab />
      )}
    </>
  );
}

function PeopleAccessTab() {
  const { enableStarletImport = false } = useTenantConfig();
  const [{ data, error }] = useQuery({
    query: PeopleAccessOverviewDocument,
    variables: { includeCredentials: enableStarletImport },
  });
  const [, sendInvitation] = useMutation(CreateInvitationDocument);
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
  const usersByEmail = React.useMemo(
    () => Map.groupBy(data?.users?.nodes ?? [], (x) => normalizeEmail(x.uEmail)),
    [data?.users?.nodes],
  );
  const report = React.useMemo(() => {
    const assignable: (AccessPerson & { matchingUserIds: string[] })[] = [];
    const invitable: (AccessPerson & { email: string })[] = [];
    const withoutEmail: AccessPerson[] = [];

    for (const person of people) {
      const hasAccount = person.userProxiesList.some(
        (x) => x.status === 'ACTIVE' && x.user,
      );
      if (hasAccount || person.personInvitationsList.length > 0) continue;

      const email = person.email?.trim();
      if (!email) {
        withoutEmail.push(person);
        continue;
      }

      const matchingUsers = usersByEmail.get(normalizeEmail(email));
      if (!matchingUsers) {
        invitable.push({ ...person, email });
      } else {
        assignable.push({ ...person, matchingUserIds: matchingUsers.map((x) => x.id) });
      }
    }

    return { assignable, invitable, withoutEmail };
  }, [people, usersByEmail]);
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
  const assignActionMap = useActionMap(personActions, report.assignable);
  const assignAccounts = useAsyncCallback(async () => {
    for (const actions of assignActionMap.values()) {
      const action = actions.find((x) => x.id === 'person.assignUserByEmail');
      if (action && 'execute' in action) await action.execute();
    }
  });
  const sendInvitations = useAsyncCallback(async () => {
    const sent = new Set<string>();
    for (const person of report.invitable) {
      const email = normalizeEmail(person.email);
      if (sent.has(email)) continue;
      sent.add(email);
      const result = await sendInvitation({
        input: { personInvitation: { personId: person.id, email: person.email } },
      });
      if (result.error) throw result.error;
    }
  });
  const hasWarnings =
    report.assignable.length > 0 ||
    report.invitable.length > 0 ||
    report.withoutEmail.length > 0;

  return (
    <>
      {error && <p className="text-danger-11">{error.message}</p>}
      {hasWarnings && (
        <div className="mb-3 grid gap-2">
          <AccessWarning
            message={`${peopleCount(report.assignable.length)} lze přiřadit k existujícímu účtu`}
            people={report.assignable}
            action={
              <SubmitButton action={assignAccounts} variant="outline">
                Přiřadit všechny
              </SubmitButton>
            }
          />
          <AccessWarning
            message={`${peopleCount(report.invitable.length)} lze pozvat e-mailem`}
            people={report.invitable}
            action={
              <SubmitButton action={sendInvitations} variant="outline">
                Pozvat všechny
              </SubmitButton>
            }
          />
          <AccessWarning
            message={`${peopleCount(report.withoutEmail.length)} nelze pozvat bez e-mailu`}
            people={report.withoutEmail}
          />
        </div>
      )}
      <div className="divide-y divide-neutral-5 rounded-md border border-neutral-5">
        <div
          className={cn(
            'hidden gap-4 bg-neutral-2 px-3 py-2 text-xs font-medium text-neutral-11 sm:grid',
            enableStarletImport
              ? 'grid-cols-[minmax(10rem,1fr)_10rem_10rem_1.75rem]'
              : 'grid-cols-[minmax(10rem,1fr)_10rem_1.75rem]',
          )}
        >
          <span>Osoba</span>
          <span className="flex items-center gap-1.5">
            <KeyRound className="size-3.5" aria-hidden="true" /> Web
          </span>
          {enableStarletImport && (
            <span className="flex items-center gap-1.5">
              <CreditCard className="size-3.5" aria-hidden="true" /> Karta
            </span>
          )}
          <span className="sr-only">Akce</span>
        </div>

        {people.map((person) => (
          <PersonAccessRow
            key={person.id}
            person={person}
            credentials={credentialsByPerson.get(person.id) ?? noCredentials}
            showCredentials={enableStarletImport}
            userActionMap={userProxyActionMap}
            invitationActionMap={invitationActionMap}
            credentialActionMap={credentialActionMap}
          />
        ))}
        {people.length === 0 && (
          <p className="px-3 py-2 text-sm text-neutral-11">Žádné osoby.</p>
        )}
      </div>
    </>
  );
}

function AccessWarning({
  message,
  people,
  action,
}: Readonly<{
  message: string;
  people: readonly Pick<PersonBasicFragment, 'id' | 'name' | 'email'>[];
  action?: React.ReactNode;
}>) {
  if (people.length === 0) return null;

  return (
    <section className="flex flex-wrap items-start gap-2 rounded-md border border-neutral-6 bg-neutral-2 px-3 py-2">
      <div className="min-w-0 basis-64 flex-1 text-sm">
        <p className="text-neutral-11">
          <strong className="text-neutral-12">{message}:</strong>{' '}
          {people.map((person, index) => (
            <React.Fragment key={person.id}>
              {index > 0 && ', '}
              <Link className="text-neutral-12 underline" href={`/clenove/${person.id}`}>
                {person.name}
              </Link>
              {person.email && <> ({person.email})</>}
            </React.Fragment>
          ))}
        </p>
      </div>
      {action && <div className="ml-auto shrink-0">{action}</div>}
    </section>
  );
}

const PersonAccessRow = React.memo(function PersonAccessRow({
  person,
  credentials,
  showCredentials,
  userActionMap,
  invitationActionMap,
  credentialActionMap,
}: Readonly<{
  person: AccessPerson;
  credentials: readonly AccessCredentialFragment[];
  showCredentials: boolean;
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
    <div
      className={cn(
        'grid grid-cols-[minmax(0,1fr)_auto] gap-x-4 gap-y-2 px-3 py-2 hover:bg-neutral-2 sm:items-center',
        showCredentials
          ? 'sm:grid-cols-[minmax(10rem,1fr)_10rem_10rem_1.75rem]'
          : 'sm:grid-cols-[minmax(10rem,1fr)_10rem_1.75rem]',
      )}
    >
      <Link className="truncate font-semibold underline" href={`/clenove/${person.id}`}>
        {person.name}
      </Link>
      <AccessMenu
        person={{
          ...person,
          canInvite: accountCount === 0 && invitations.length === 0,
        }}
        credentials={credentials}
        showCredentials={showCredentials}
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
              <span className="truncate">Pozvánka odeslána</span>
            </span>
          ) : hasOnlyExpiredAccounts ? (
            <span className="ml-1 flex min-w-0 items-center gap-1 text-danger-10">
              <span className="truncate">Přístup odebrán</span>
            </span>
          ) : person.email ? (
            <span className="ml-1 flex min-w-0 items-center gap-1 text-danger-10">
              <span className="truncate">Bez pozvánky</span>
            </span>
          ) : (
            <span className="ml-1 flex min-w-0 items-center gap-1 text-danger-10">
              <span className="truncate">Chybí e-mail</span>
            </span>
          )}
        </div>
      </div>
      {showCredentials && (
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
      )}
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
  showCredentials,
  userActionMap,
  invitationActionMap,
  credentialActionMap,
}: Readonly<{
  person: AccessPerson & { canInvite: boolean };
  credentials: readonly AccessCredentialFragment[];
  showCredentials: boolean;
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
      className={cn(
        'col-start-2 row-start-1 ml-auto',
        showCredentials ? 'sm:col-start-4' : 'sm:col-start-3',
      )}
      actions={actions}
      variant="row"
      align="end"
    />
  );
}
