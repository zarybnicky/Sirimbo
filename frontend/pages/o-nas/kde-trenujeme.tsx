import * as React from 'react';
import { LocationCard } from 'components/cards/LocationCard';
import { CallToAction } from 'components/CallToAction';
import { Heading } from 'components/Heading';
import { getPlaceholder } from 'lib/get-placeholder';
import { Layout } from 'components/layout/Layout';

export default function LocationsPage() {
  const items = useLocations();
  return (
    <>
      <Heading color={{ r: 20, g: 200, b: 20, a: 0.5 }} text="Kde trénujeme" image="" />
      <div className="container mx-auto max-w-3xl mt-8 mb-8">
        {items.map((x, i) => (
          <LocationCard item={x} key={i} />
        ))}
      </div>
      <CallToAction />
    </>
  );
}

const useLocations = () => [
  {
    image: 'https://tkolymp.cz/fotogalerie/223/foto/10089',
    name: 'Taneční centrum při FZŠ Holečkova',
    address: 'Holečkova 10, 779 00, Olomouc (vchod brankou u zastávy Povel - škola)',
    href: 'https://www.zsholeckova.cz/',
    mapHref: 'https://goo.gl/maps/swv3trZB2uvjcQfR6',
    map: {
      lat: 49.57963,
      lng: 17.2495939,
      zoom: 12,
    },
  },
  {
    image: getPlaceholder(360, 240),
    name: 'Tělocvična Slovanského gymnázia',
    address: 'Jiřího z Poděbrad 13, 779 00 Olomouc (vchod brankou z ulice U reálky)',
    href: 'https://www.sgo.cz/',
    mapHref: 'https://goo.gl/maps/PgsEra8TnYV4V7KGA',
    map: {
      lat: 49.5949,
      lng: 17.2634,
      zoom: 12,
    },
  },
  {
    image: getPlaceholder(360, 240),
    name: 'Taneční sál Gala',
    address: 'Západní 1, 796 04 Prostějov-Krasice (vchod vedle podnikové prodejny Gala)',
    href: null,
    mapHref: 'https://goo.gl/maps/Jtv6mdoSgBEdsiTN7',
    map: {
      lat: 49.4681836,
      lng: 17.0837344,
      zoom: 12,
    },
  },
];

LocationsPage.getLayout = (page: React.ReactElement) => (
  <Layout showTopMenu>{page}</Layout>
);
