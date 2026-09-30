import { PublicLocationsDocument } from '@/graphql/Location';
import { executeGraphql } from '@/lib/server/graphql';
import { LocationMap } from '@/ui/LocationMap';
import { LocationAddress } from '@/ui/LocationAddress';
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
  const preview = await executeGraphql(PublicLocationsDocument);
  const locationsList = preview?.tenant?.locationsList ?? [];

  return (
    <>
      <PageHeader title="Kde trénujeme" />

      <div className="my-8 space-y-12">
        {locationsList.map((loc) => {
          const image = loc.coverImage ?? loc.imagesList.find((x) => x.file)?.file;

          return (
            <section key={loc.id}>
              <h2 className="mb-4 text-2xl font-bold text-accent-10">
                <Link
                  href={`/lokality/${loc.id}`}
                  className="hover:underline focus-visible:underline"
                >
                  {loc.longName || loc.name}
                </Link>
              </h2>

              <div className="grid items-center gap-6 md:grid-cols-[auto_1fr]">
                {loc.latitude != null && loc.longitude != null && (
                  <LocationMap map={{ lat: loc.latitude, lng: loc.longitude, zoom: 16 }} />
                )}
                <div className="text-neutral-12">
                  <LocationAddress
                    address={loc.address}
                    latitude={loc.latitude}
                    longitude={loc.longitude}
                  />
                  <RichTextView value={loc.description} className="max-w-none" />
                </div>
              </div>

              {image && (
                <div className="relative mt-4 aspect-2/1 max-h-96 overflow-hidden rounded-md bg-neutral-3">
                  <Image
                    fill
                    unoptimized
                    src={image.url}
                    alt={`Fotografie místa ${loc.longName || loc.name}`}
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
    </>
  );
}
