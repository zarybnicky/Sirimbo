'use client';

import { AccessEventOverviewDocument } from '@/graphql/AccessCredential';
import { mifareCodeToLabel } from '@/lib/access-credentials';
import { useTenantConfig } from '@/lib/auth';
import { dateTimeFormatter } from '@/ui/format';
import { AccessCredentialForm } from '@/ui/forms/AccessCredentialForm';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/PageHeader';
import { PeopleAccessOverview } from '@/ui/PeopleAccessOverview';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@/ui/dialog';
import Link from 'next/link';
import { parseAsString, useQueryState } from 'nuqs';
import * as React from 'react';
import { useQuery } from 'urql';

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
        <TabMenu selected={tab} onSelect={setTab}>
          <Tab id="people" title="Osoby">
            <PeopleAccessOverview />
          </Tab>
          <Tab id="events" title="Přístupy">
            <EventsTab />
          </Tab>
        </TabMenu>
      ) : (
        <PeopleAccessOverview />
      )}
    </>
  );
}

const EventsTab = React.memo(function EventsTab() {
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
            {event.person ? (
              <Link className="underline" href={`/clenove/${event.person.id}`}>
                {event.person.name}
              </Link>
            ) : (
              <>
                {mifareCodeToLabel(event.code)}
                <code className="ml-2 text-neutral-11">{event.code}</code>
              </>
            )}
          </span>
          <span className="ml-auto text-neutral-11">
            {dateTimeFormatter.format(new Date(event.occurredAt))}
            {event.location && (
              <>
                {' · '}
                <Link className="underline" href={`/lokality/${event.location.id}`}>
                  {event.location.name}
                </Link>
              </>
            )}
            {' · '}
            {event.device}
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
});
