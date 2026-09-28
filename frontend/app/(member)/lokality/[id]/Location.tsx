'use client';

import { LocationPageDocument, type LocationPageQuery } from '@/graphql/Location';
import { useActions } from '@/lib/actions';
import { locationActions } from '@/lib/actions/location';
import { mifareCodeToLabel } from '@/lib/access-credentials';
import { EventButton } from '@/ui/EventButton';
import { dateTimeFormatter } from '@/ui/format';
import { RichTextView } from '@/ui/RichTextView';
import { badgeCls } from '@/ui/style';
import { PageHeader } from '@/ui/TitleBar';
import { ExternalLink, MapPin } from 'lucide-react';
import Image from 'next/image';
import Link from 'next/link';
import * as React from 'react';
import { useQuery } from 'urql';
import { useAuth } from '@/lib/auth';
import type { AddressDomain } from '@/graphql';

export function Location({
  initialData,
  start,
}: {
  initialData: LocationPageQuery;
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
    query: LocationPageDocument,
    variables: {
      id: initialData.location!.id,
      start: new Date(now).toISOString(),
    },
  });
  const result = data ?? initialData;
  const location = result.location!;
  const actions = useActions(locationActions, location);
  const ongoingEvents = (result.events ?? []).filter(
    (x) => new Date(x.since).getTime() <= now && new Date(x.until).getTime() > now,
  );
  const upcomingEvents = (result.events ?? []).filter(
    (x) => new Date(x.since).getTime() > now,
  );
  const coverImage = location.coverImage;
  const images = location.imagesList.flatMap((x) =>
    x.file && x.file.id !== coverImage?.id ? [x.file] : [],
  );

  return (
    <>
      <PageHeader
        title={location.longName || location.name}
        subtitle={
          auth.isAdmin ? (
            <span className={badgeCls()}>
              {location.showInLists ? 'Viditelná' : 'Skrytá'}
            </span>
          ) : undefined
        }
        actions={actions}
        breadcrumbs={[{ label: 'Klub', href: '/tanecni-klub' }, { label: location.name }]}
      />

      <h2 className="mb-2 text-lg font-bold">Právě probíhá</h2>
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
          <Link
            href={`/rozpis?location=${location.id}`}
            className="mt-2 inline-block text-sm text-neutral-11 underline"
          >
            Zobrazit další
          </Link>
        </>
      )}

      <h2 className="mt-4 mb-2 text-lg font-bold">O místě</h2>
      {location.description?.trim() ? (
        <RichTextView className="mb-2" value={location.description} />
      ) : (
        <p className="text-sm mb-2 text-neutral-10">(Nevyplněno)</p>
      )}

      <LocationAddress
        address={location.address}
        latitude={location.latitude}
        longitude={location.longitude}
      />

      <h2 className="mt-4 mb-2 text-lg font-bold">Fotografie</h2>
      {coverImage && (
        <a
          href={coverImage.url}
          target="_blank"
          rel="noreferrer"
          aria-label={`Otevřít fotografii místa ${location.name}`}
          className="relative mb-2 block aspect-2/1 max-h-128 overflow-hidden rounded-md bg-neutral-3"
        >
          <Image
            fill
            unoptimized
            src={coverImage.url}
            alt=""
            sizes="100vw"
            className="object-cover transition-transform hover:scale-102"
          />
        </a>
      )}

      {images.length > 0 && (
        <div className="mb-2 grid grid-cols-2 gap-2 md:grid-cols-3">
          {images.map((image) => (
            <a
              key={image.id}
              href={image.url}
              target="_blank"
              rel="noreferrer"
              aria-label={`Otevřít fotografii místa ${location.name}`}
              className="relative aspect-4/3 overflow-hidden rounded-md bg-neutral-3"
            >
              <Image
                fill
                unoptimized
                src={image.url}
                alt=""
                sizes="(min-width: 768px) 33vw, 50vw"
                className="object-cover transition-transform hover:scale-102"
              />
            </a>
          ))}
        </div>
      )}

      {!coverImage && images.length === 0 && (
        <p className="text-sm text-neutral-10">Nejsou nahrané žádné fotografie.</p>
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

function LocationAddress({ address, latitude, longitude }: {
  address: AddressDomain | null;
  latitude: number | null;
  longitude: number | null;
}) {
  const number = [address?.conscriptionNumber, address?.orientationNumber]
    .filter(Boolean)
    .join('/');
  const street = [address?.street, number].filter(Boolean).join(' ');
  const city = [address?.postalCode, address?.city].filter(Boolean).join(' ');
  const mapQuery = latitude != null && longitude != null
    ? `${latitude},${longitude}`
    : [street, address?.district, city, address?.region]
        .filter(Boolean)
        .join(', ');

  if (!mapQuery) {
    return <p className="text-sm text-neutral-10">(Adresa nevyplněna)</p>;
  }

  return (
    <div className="mb-4 flex items-start gap-2 text-sm">
      <MapPin className="mt-0.5 size-4 shrink-0 text-accent-11" aria-hidden="true" />
      <address className="not-italic">
        {street && <div>{street}</div>}
        {address?.district && <div>{address.district}</div>}
        {city && <div>{city}</div>}
        {address?.region && <div>{address.region}</div>}
        <a
          className="mt-1 inline-flex items-center gap-1 underline"
          href={`https://www.google.com/maps/search/?api=1&query=${encodeURIComponent(mapQuery)}`}
          target="_blank"
          rel="noreferrer"
        >
          Otevřít v mapě
          <ExternalLink className="size-3" aria-hidden="true" />
        </a>
      </address>
    </div>
  );
}
