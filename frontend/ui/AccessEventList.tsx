'use client';

import { DotSeparated } from '@/ui/DotSeparated';

import { AccessEventOverviewDocument } from '@/graphql/AccessCredential';
import { mifareCodeToLabel } from '@/lib/access-credentials';
import { useAuth } from '@/lib/auth';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@/ui/dialog';
import { numericFullFormatter } from '@/ui/format';
import { AccessCredentialForm } from '@/ui/forms/AccessCredentialForm';
import { PowerIcon } from 'lucide-react';
import Link from 'next/link';
import * as React from 'react';
import { useQuery } from 'urql';

export function AccessEventList({ locationId }: { locationId?: string }) {
  const auth = useAuth();
  const [{ data, error }] = useQuery({
    query: AccessEventOverviewDocument,
    variables: {
      condition: locationId ? { locationId } : undefined,
      first: locationId ? 20 : 100,
    },
  });
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
            <b className="tabular-nums leading-tight pe-2">
              {numericFullFormatter.format(new Date(event.occurredAt))}
            </b>
            {event.person ? (
              <Link className="underline" href={`/clenove/${event.person.id}`}>
                {event.person.name}
              </Link>
            ) : event.code !== '00000000' ? (
              <>
                {mifareCodeToLabel(event.code)}
                <code className="ml-2 text-neutral-11">{event.code}</code>
              </>
            ) : (
              <PowerIcon className="inline size-3 -mt-0.75 text-accent-11" />
            )}
          </span>
          <DotSeparated className="ml-auto text-neutral-11 tabular-nums">
            {event.allowed ? 'Povoleno' : 'Zamítnuto'}
            {event.location && (
              <Link className="underline" href={`/lokality/${event.location.id}`}>
                {event.location.name}
              </Link>
            )}
          </DotSeparated>
          {auth.isAdmin &&
            !event.allowed &&
            !allowedCodes.has(`${event.kind}:${event.code}`) && (
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
