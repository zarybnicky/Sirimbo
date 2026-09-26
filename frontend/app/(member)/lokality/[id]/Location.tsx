'use client';

import {
  TenantLocationPageDocument,
  type TenantLocationPageQuery,
} from '@/graphql/Tenant';
import { useActions } from '@/lib/actions';
import { tenantLocationActions } from '@/lib/actions/tenantLocation';
import { mifareCodeToLabel } from '@/lib/access-credentials';
import { EventButton } from '@/ui/EventButton';
import { dateTimeFormatter } from '@/ui/format';
import { RichTextView } from '@/ui/RichTextView';
import { badgeCls } from '@/ui/style';
import { PageHeader } from '@/ui/TitleBar';
import { ExternalLink, MapPin } from 'lucide-react';
import Link from 'next/link';
import * as React from 'react';
import { useQuery } from 'urql';
import { useAuth } from '@/lib/auth';
import type { AddressDomain } from '@/graphql';

export function Location({
  initialData,
  start,
}: {
  initialData: TenantLocationPageQuery;
  start: string;
}) {
  const auth = useAuth();
  const [now, setNow] = React.useState(() => new Date(start).getTime());
  React.useEffect(() => {
    const update = () => setNow(Date.now());
    update();
    const interval = setInterval(update, 60_000);
    return () => clearInterval(interval);
  }, []);

  const [{ data }] = useQuery({
    query: TenantLocationPageDocument,
    variables: {
      id: initialData.tenantLocation!.id,
      start: new Date(now).toISOString(),
    },
  });
  const result = data ?? initialData;
  const location = result.tenantLocation!;
  const events = result.events ?? [];
  const actions = useActions(tenantLocationActions, location);
  const ongoingEvents = events.filter(
    (x) => new Date(x.since).getTime() <= now && new Date(x.until).getTime() > now,
  );
  const upcomingEvents = events.filter((x) => new Date(x.since).getTime() > now);

  return (
    <>
      <PageHeader
        title={location.name}
        subtitle={
          auth.isAdmin ? (
            <span className={badgeCls()}>
              {location.isPublic ? 'Veřejná' : 'Neveřejná'}
            </span>
          ) : undefined
        }
        actions={actions}
        breadcrumbs={[{ label: 'Klub', href: '/tanecni-klub' }, { label: location.name }]}
      />

      <h2 className="mb-2 text-lg font-bold">Adresa</h2>
      {location.address ? (
        <LocationAddress address={location.address} />
      ) : (
        <p className="text-sm text-neutral-10">Adresa není vyplněná.</p>
      )}
      <RichTextView value={location.description} />

      <h2 className="mt-4 mb-2 text-lg font-bold">Právě probíhá</h2>
      {ongoingEvents.length > 0 ? (
        <div className="flex flex-col gap-1">
          {ongoingEvents.map((event) => (
            <EventButton key={event.id} instance={event} viewer="auto" showDate />
          ))}
        </div>
      ) : (
        <p className="text-sm text-neutral-10">Nic právě neprobíhá.</p>
      )}

      {upcomingEvents.length > 0 && (
        <>
          <h2 className="mt-4 mb-2 text-lg font-bold">Nadcházející</h2>
          <div className="flex flex-col gap-1">
            {upcomingEvents.map((event) => (
              <EventButton key={event.id} instance={event} viewer="auto" showDate />
            ))}
          </div>
        </>
      )}

      {location.accessEventsList.length > 0 && (
        <>
          <h2 className="mt-4 mb-2 text-lg font-bold">Poslední průchody</h2>
          <div className="divide-y divide-neutral-5 rounded-md border border-neutral-5">
            {location.accessEventsList.map((event) => (
              <div
                key={event.id}
                className="flex flex-wrap items-center justify-between gap-2 px-3 py-2 text-sm"
              >
                <span>
                  <b className={event.allowed ? 'text-green-11' : 'text-danger-11'}>
                    {event.allowed ? 'Povoleno' : 'Zamítnuto'}
                  </b>
                  {' · '}
                  {event.person ? (
                    <Link className="underline" href={`/clenove/${event.person.id}`}>
                      {event.person.name}
                    </Link>
                  ) : (
                    <>
                      {mifareCodeToLabel(event.code)}{' '}
                      <code className="text-neutral-11">{event.code}</code>
                    </>
                  )}
                </span>
                <span className="ml-auto text-neutral-11">
                  {dateTimeFormatter.format(new Date(event.occurredAt))}
                  {' · '}
                  {event.device}
                  {event.reason && ` · ${event.reason}`}
                </span>
              </div>
            ))}
          </div>
        </>
      )}
    </>
  );
}

function LocationAddress({ address }: { address: AddressDomain }) {
  const number = [address.conscriptionNumber, address.orientationNumber]
    .filter(Boolean)
    .join('/');
  const street = [address.street, number].filter(Boolean).join(' ');
  const city = [address.postalCode, address.city].filter(Boolean).join(' ');
  const mapQuery = [street, address.district, city, address.region]
    .filter(Boolean)
    .join(', ');

  return (
    <div className="mb-4 flex items-start gap-2 text-sm">
      <MapPin className="mt-0.5 size-4 shrink-0 text-accent-11" aria-hidden="true" />
      <address className="not-italic">
        {street && <div>{street}</div>}
        {address.district && <div>{address.district}</div>}
        {city && <div>{city}</div>}
        {address.region && <div>{address.region}</div>}
        {mapQuery && (
          <a
            className="mt-1 inline-flex items-center gap-1 underline"
            href={`https://www.google.com/maps/search/?api=1&query=${encodeURIComponent(mapQuery)}`}
            target="_blank"
            rel="noreferrer"
          >
            Otevřít v mapě
            <ExternalLink className="size-3" aria-hidden="true" />
          </a>
        )}
      </address>
    </div>
  );
}
