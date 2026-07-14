/* eslint-disable import-x/no-unused-modules */
import { publicPageMetadata } from '@/lib/server/seo';
import { PageHeader } from '@/ui/TitleBar';
import Image from 'next/image';
import { Metadata } from 'next';
import { LeafletMap } from '@/ui/LeafletMap';

export const generateMetadata = (): Promise<Metadata> =>
  publicPageMetadata({
    title: 'Kde trénujeme',
    description:
      'Přehled tanečních sálů TK Olymp v Olomouci: Taneční centrum při FZŠ Holečkova a tělocvična Slovanského gymnázia včetně adres a map.',
    path: '/kde-trenujeme',
  });

export default function LocationsPage() {
  return (
    <>
      <PageHeader title="Kde trénujeme" />

      <div className="mt-8 mb-16 space-y-4">
        <h2 className="text-4xl text-accent-10 tracking-wide">V Olomouci</h2>
        <LocationCard
          name="Taneční centrum při FZŠ Holečkova"
          href="https://www.zsholeckova.cz/"
          mapHref="https://goo.gl/maps/swv3trZB2uvjcQfR6"
          map={{ lat: 49.579_63, lng: 17.249_593_9, zoom: 12 }}
        >
          Holečkova 10, 779 00, Olomouc
          <br />
          (vchod brankou u zastávky Povel – škola)
        </LocationCard>

        <div className="min-h-[200px] md:min-h-[300px] my-4 relative">
          <Image
            className="object-cover"
            alt="Taneční sál v Tanečním centru při FZŠ Holečkova v Olomouci"
            src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687512915639-Saly-Holeckova.jpg"
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

        <div className="min-h-[200px] md:min-h-[300px] my-4 relative">
          <Image
            className="object-cover"
            alt="Tělocvična Slovanského gymnázia používaná pro tréninky TK Olymp"
            src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687512915639-Saly-SGO.jpg"
            fill
            sizes="(min-width: 600px) 520px, calc(100vw - 1rem)"
          />
        </div>
      </div>
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
