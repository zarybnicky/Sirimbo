import { PublicLocationsDocument } from '@/graphql/Location';
import { executeGraphql } from '@/lib/server/graphql';
import { getRequestContext } from '@/lib/server/tenant';
import { LeafletMap } from '@/ui/LeafletMap';
import { RichTextView } from '@/ui/RichTextView';
import { PageHeader } from '@/ui/TitleBar';
import type { Metadata } from 'next';
import Image from 'next/image';
import Link from 'next/link';

export const metadata: Metadata = {
  title: 'Kde trénujeme',
  description:
    'Přehled tanečních sálů TK Olymp v Olomouci: Taneční centrum při FZŠ Holečkova a tělocvična Slovanského gymnázia včetně adres a map.',
  alternates: { canonical: '/kde-trenujeme' },
};

export default async function LocationsPage() {
  const { auth } = await getRequestContext();
  const preview = auth.isAdmin ? await executeGraphql(PublicLocationsDocument) : null;
  const locationsList = preview?.tenant?.locationsList ?? [];

  return (
    <>
      <PageHeader title="Kde trénujeme" />

      <div className="mt-8 mb-16 space-y-4">
        <h2 className="text-4xl text-accent-10 tracking-wide">V Olomouci</h2>
        <LocationCard
          name="Taneční centrum při FZŠ Holečkova"
          href="https://www.zsholeckova.cz/"
          mapHref="https://goo.gl/maps/swv3trZB2uvjcQfR6"
          map={{ lat: 49.57963, lng: 17.2495939, zoom: 12 }}
        >
          Holečkova 10, 779 00, Olomouc
          <br />
          (vchod brankou u zastávky Povel – škola)
        </LocationCard>

        <div className="min-h-50 md:min-h-75 my-4 relative">
          <Image
            className="object-cover"
            alt="Taneční sál v Tanečním centru při FZŠ Holečkova v Olomouci"
            src="/f/23/Saly-Holeckova.jpg"
            fill
            sizes="(min-width: 600px) 520px, calc(100vw - 1rem)"
          />
        </div>

        <LocationCard
          name="Tělocvična Slovanského gymnázia"
          href="https://www.sgo.cz/"
          mapHref="https://goo.gl/maps/PgsEra8TnYV4V7KGA"
          map={{ lat: 49.5949, lng: 17.2634, zoom: 12 }}
        >
          Jiřího z Poděbrad 13, 779 00 Olomouc
          <br />
          (vchod brankou z ulice U reálky)
        </LocationCard>

        <div className="min-h-50 md:min-h-75 my-4 relative">
          <Image
            className="object-cover"
            alt="Tělocvična Slovanského gymnázia používaná pro tréninky TK Olymp"
            src="/f/21/Saly-SGO.jpg"
            fill
            sizes="(min-width: 600px) 520px, calc(100vw - 1rem)"
          />
        </div>
      </div>

      {auth.isAdmin && (
        <div className="my-8 space-y-12">
          <h2 className="text-2xl font-bold">Náhled míst z databáze</h2>
          {locationsList.map((location) => {
            const title = location.longName || location.name;
            const address = location.address;
            const number = [address?.conscriptionNumber, address?.orientationNumber]
              .filter(Boolean)
              .join('/');
            const street = [address?.street, number].filter(Boolean).join(' ');
            const city = [address?.postalCode, address?.city].filter(Boolean).join(' ');
            const image =
              location.coverImage ?? location.imagesList.find((item) => item.file)?.file;
            const hasCoordinates =
              location.latitude != null && location.longitude != null;

            return (
              <section key={location.id}>
                <h2 className="mb-4 text-2xl font-bold text-accent-10">
                  <Link href={`/lokality/${location.id}`} className="underline">
                    {title}
                  </Link>
                </h2>

                <div className="grid gap-6 md:grid-cols-[auto_1fr]">
                  {hasCoordinates && (
                    <LeafletMap
                      name={title}
                      map={{
                        lat: location.latitude!,
                        lng: location.longitude!,
                        zoom: 16,
                      }}
                    />
                  )}
                  <div className="space-y-3 text-neutral-12">
                    {address && (
                      <address className="not-italic">
                        {street && <div>{street}</div>}
                        {address.district && <div>{address.district}</div>}
                        {city && <div>{city}</div>}
                        {address.region && <div>{address.region}</div>}
                      </address>
                    )}
                    <RichTextView value={location.description} />
                    {hasCoordinates && (
                      <a
                        href={`https://www.openstreetmap.org/?mlat=${location.latitude}&mlon=${location.longitude}#map=16/${location.latitude}/${location.longitude}`}
                        target="_blank"
                        rel="noreferrer"
                        className="underline"
                      >
                        Otevřít mapu
                      </a>
                    )}
                  </div>
                </div>

                {image && (
                  <div className="relative mt-4 aspect-2/1 max-h-96 overflow-hidden rounded-md bg-neutral-3">
                    <Image
                      fill
                      unoptimized
                      src={image.url}
                      alt={`Fotografie místa ${title}`}
                      sizes="(min-width: 768px) 800px, 100vw"
                      className="object-cover"
                    />
                  </div>
                )}
              </section>
            );
          })}
          {locationsList.length === 0 && (
            <p className="text-neutral-11">Zatím tu nejsou žádná místa.</p>
          )}
        </div>
      )}
    </>
  );
}

type Props = {
  name: string;
  children: React.ReactNode;
  href: string;
  mapHref: string;
  map: {
    lat: number;
    lng: number;
    zoom: number;
  };
};

function LocationCard(x: Props) {
  return (
    <div>
      <h3 className="text-accent-10 text-2xl font-bold mb-4 mt-8">{x.name}</h3>
      <div className="grid md:grid-cols-[1fr_2fr] gap-4 items-center">
        <LeafletMap map={x.map} name={x.name} />

        <div className="grow text-neutral-12">
          <div className="py-2">{x.children}</div>
          <a href={x.href} rel="noreferrer" target="_blank" className="block underline">
            {x.href}
          </a>
          <a
            href={x.mapHref}
            rel="noreferrer"
            target="_blank"
            className="block underline"
          >
            Otevřít mapu
          </a>
        </div>
      </div>
    </div>
  );
}
