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
import { dateTimeFormatter } from '@/ui/format';
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
    for (const x of credentials) {
      if (!x.person) continue;
      const personCredentials = result.get(x.person.id) ?? [];
      personCredentials.push(x);
      result.set(x.person.id, personCredentials);
    }
    return result;
  }, [credentials]);
  const allowedCodes = new Set(
    credentials
      .filter((x) => x.isAllowed)
      .map((x) => `${x.kind}:${x.code}`),
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
              {personCredentials.map((x) => (
                <ActionRow key={x.id} actions={actionMap.get(x.id)!}>
                  <div className="grow gap-3 align-baseline flex flex-wrap justify-between text-sm py-1">
                    <span className={x.isAllowed ? '' : 'line-through'}>
                      <b>{x.label}</b>
                      <code className="ml-2 text-neutral-11">({x.code})</code>
                    </span>
                    <span className="text-right text-neutral-11">
                      {x.lastUsed
                        ? `Naposledy ${dateTimeFormatter.format(new Date(x.lastUsed))}`
                        : 'Nikdy nepoužita'}
                    </span>
                  </div>
                </ActionRow>
              ))}
              {personCredentials.length === 0 && (
                <div className="flex gap-3 items-center text-sm text-neutral-11">
                  <div>Bez přístupové karty</div>
                  <AssignCard personId={person.id} people={people} />
                </div>
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
  personId: initialPersonId,
  people,
}: {
  event?: AccessEventFragment;
  personId?: string;
  people: { id: string; label: string }[];
}) {
  const [personId, setPersonId] = React.useState<string | null>();
  const finalPersonId = initialPersonId ?? personId;
  return (
    <Dialog>
      <DialogTrigger size="xs" text="Přidat" />
      <DialogContent>
        <DialogTitle>
          {event ? `Přiřadit kartu ${mifareCodeToLabel(event.code)}` : 'Přidat kartu'}
        </DialogTitle>
        {!initialPersonId && (
          <Combobox
            value={personId}
            onChange={setPersonId}
            options={people}
            label="Osoba"
            placeholder="Vyberte osobu"
          />
        )}
        {finalPersonId && (
          <CreateAccessCredentialForm
            key={personId}
            personId={finalPersonId}
            initialValue={event ? {
              label: mifareCodeToLabel(event.code),
              code: event.code,
            } : undefined}
          />
        )}
      </DialogContent>
    </Dialog>
  );
}
