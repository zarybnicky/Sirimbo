'use client';

import { LocationDocument, type LocationFragment } from '@/graphql/Location';
import type { EventWithTrainerFragment } from '@/graphql/Event';
import { useActions } from '@/lib/actions';
import { locationActions } from '@/lib/actions/location';
import { AccessEventList } from '@/ui/AccessEventList';
import { EventButton } from '@/ui/EventButton';
import { LocationMap } from './LocationMap.tsx';
import { LocationAddress } from './LocationAddress.tsx';
import { RichTextView } from '@/ui/RichTextView';
import { badgeCls } from '@/ui/style';
import { PageHeader } from '@/ui/PageHeader';
import { Tab, TabMenu } from '@/ui/TabMenu';
import Image from '@/ui/Image';
import Link from 'next/link';
import { parseAsString, useQueryState } from 'nuqs';
import * as React from 'react';
import { useQuery } from 'urql';
import { useAuth } from '@/lib/auth';

export function Location({
  initialLocation,
  initialEvents,
  start,
}: {
  initialLocation: LocationFragment;
  initialEvents: readonly EventWithTrainerFragment[];
  start: string;
}) {
  const auth = useAuth();
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );
  const [now, setNow] = React.useState(() => new Date(start).getTime());
  React.useEffect(() => {
    const update = () => setNow(Date.now());
    update();
    const interval = setInterval(update, 60_000);
    return () => clearInterval(interval);
  }, []);

  const [{ data }] = useQuery({
    query: LocationDocument,
    variables: {
      id: initialLocation.id,
      eventsCondition: {
        overlaps: { start: new Date(now).toISOString() },
        locationId: initialLocation.id,
      },
      withEvents: true,
    },
  });
  const location = data?.location ?? initialLocation;
  const events = data?.eventsList ?? initialEvents;
  const actions = useActions(locationActions, location);
  const ongoingEvents = events.filter(
    (x) => new Date(x.since).getTime() <= now && new Date(x.until).getTime() > now,
  );
  const upcomingEvents = events.filter((x) => new Date(x.since).getTime() > now);
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
        breadcrumbLabel="Místo"
        breadcrumbs={[{ label: 'Klub', href: '/tanecni-klub' }]}
      />

      <TabMenu selected={tab} onSelect={setTab}>
        <Tab id="overview" title="Přehled">
          <h2 className="mb-2 text-lg font-bold">Právě probíhá</h2>
          {ongoingEvents.length > 0 ? (
            <div className="flex flex-col gap-1">
              {ongoingEvents.map((event) => (
                <EventButton key={event.id} event={event} viewer="auto" showDate />
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
                  <EventButton key={event.id} event={event} viewer="auto" showDate />
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

          <h2 className="mt-4 mb-2 text-lg font-bold">O místu</h2>
          <div
            className={
              location.latitude && location.longitude
                ? 'grid items-center gap-6 md:grid-cols-[auto_1fr]'
                : ''
            }
          >
            {location.latitude && location.longitude && (
              <LocationMap
                map={{ lat: location.latitude, lng: location.longitude, zoom: 14 }}
              />
            )}
            <div>
              {location.description?.trim() ? (
                <RichTextView className="mb-2" value={location.description} />
              ) : (
                <p className="text-sm mb-2 text-neutral-10">(Nevyplněno)</p>
              )}
              <LocationAddress
                address={location.address}
                latitude={location.latitude}
                longitude={location.longitude}
                showMissingAddress={auth.isAdmin}
              />
            </div>
          </div>

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
        </Tab>

        <Tab id="access" title="Přístupy" requireStarletImport>
          <AccessEventList locationId={location.id} />
        </Tab>
      </TabMenu>
    </>
  );
}
