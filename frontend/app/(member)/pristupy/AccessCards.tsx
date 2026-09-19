'use client';

import {
  AccessCredentialOverviewDocument,
  type AccessEventFragment,
} from '@/graphql/AccessCredential';
import { useActionMap } from '@/lib/actions';
import { accessCredentialActions } from '@/lib/actions/accessCredential';
import { mifareCodeToLabel } from '@/lib/access-credentials';
import { ActionRow } from '@/ui/ActionRow';
import { Combobox } from '@/ui/fields/Combobox';
import { dateTimeFormatter, formatOpenDateRange } from '@/ui/format';
import { CreateAccessCredentialForm } from '@/ui/forms/CreateAccessCredentialForm';
import { TabMenu } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@/ui/dialog';
import Link from 'next/link';
import { parseAsString, useQueryState } from 'nuqs';
import * as React from 'react';
import { useQuery } from 'urql';

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
  const actionMap = useActionMap(accessCredentialActions, credentials);
  const people = React.useMemo(
    () =>
      (data?.people?.nodes ?? [])
        .map((person) => ({ id: person.id, label: person.name }))
        .toSorted((a, b) => a.label.localeCompare(b.label)),
    [data?.people?.nodes],
  );
  const credentialsByPerson = React.useMemo(() => {
    const result = new Map<string, (typeof credentials)[number][]>();
    for (const credential of credentials) {
      if (!credential.person) continue;
      const personCredentials = result.get(credential.person.id) ?? [];
      personCredentials.push(credential);
      result.set(credential.person.id, personCredentials);
    }
    return result;
  }, [credentials]);
  const allowedCodes = new Set(
    credentials
      .filter((credential) => credential.isAllowed)
      .map((credential) => `${credential.kind}:${credential.code}`),
  );
  const events = data?.accessEventsList ?? [];

  const peopleTab = (
    <div className="divide-y divide-neutral-5 rounded-md border border-neutral-5">
      {people.map((person) => {
        const personCredentials = credentialsByPerson.get(person.id) ?? [];
        return (
          <div
            key={person.id}
            className="grid gap-2 px-3 py-2 sm:grid-cols-[minmax(10rem,1fr)_2fr]"
          >
            <Link className="font-semibold underline" href={`/clenove/${person.id}`}>
              {person.label}
            </Link>
            <div className="space-y-1">
              {personCredentials.map((credential) => (
                <ActionRow key={credential.id} actions={actionMap.get(credential.id)!}>
                  <div className="flex grow flex-wrap items-baseline justify-between gap-2 py-1 text-sm">
                    <span className={credential.isAllowed ? '' : 'line-through'}>
                      <b>{credential.label}</b>
                      <code className="ml-2 text-neutral-11">{credential.code}</code>
                    </span>
                    <span className="text-right text-neutral-11">
                      <span>{formatOpenDateRange(credential)}</span>
                      <span className="block">
                        {credential.lastUsed
                          ? `Naposledy ${dateTimeFormatter.format(new Date(credential.lastUsed))}`
                          : 'Nikdy nepoužita'}
                      </span>
                    </span>
                  </div>
                </ActionRow>
              ))}
              {personCredentials.length === 0 && (
                <span className="text-sm text-neutral-11">Bez přístupové karty</span>
              )}
            </div>
          </div>
        );
      })}
      {people.length === 0 && (
        <p className="px-3 py-2 text-sm text-neutral-11">Žádné osoby.</p>
      )}
    </div>
  );

  const eventsTab = (
    <div className="divide-y divide-neutral-5 rounded-md border border-neutral-5">
      {events.map((event) => {
        const allowed = allowedCodes.has(`${event.kind}:${event.code}`);
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
            {!event.allowed && !allowed && <AssignCard event={event} people={people} />}
          </div>
        );
      })}
      {events.length === 0 && (
        <p className="px-3 py-2 text-sm text-neutral-11">Žádné události.</p>
      )}
    </div>
  );

  return (
    <>
      <PageHeader title="Přístupy" />
      {error && <p className="text-danger-11">{error.message}</p>}
      <TabMenu
        selected={tab}
        onSelect={setTab}
        options={[
          { id: 'people', title: 'Osoby a karty', contents: () => peopleTab },
          { id: 'events', title: 'Události', contents: () => eventsTab },
        ]}
      />
    </>
  );
}

function AssignCard({
  event,
  people,
}: {
  event: AccessEventFragment;
  people: { id: string; label: string }[];
}) {
  const [personId, setPersonId] = React.useState<string | null>();
  return (
    <Dialog>
      <DialogTrigger size="sm" text="Přiřadit" />
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
          <CreateAccessCredentialForm
            key={personId}
            personId={personId}
            initialValue={{
              kind: event.kind,
              label: mifareCodeToLabel(event.code),
              code: event.code,
            }}
          />
        )}
      </DialogContent>
    </Dialog>
  );
}
