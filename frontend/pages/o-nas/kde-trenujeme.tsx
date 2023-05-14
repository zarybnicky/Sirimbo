import * as React from 'react';
import { LocationCard } from 'components/cards/LocationCard';
import { CallToAction } from 'components/CallToAction';
import { Heading } from 'components/Heading';
import { Layout } from 'components/layout/Layout';

export default function LocationsPage() {
  return (
    <>
      <Heading>Kde trénujeme</Heading>

      <div className="my-16 flex flex-col gap-4">
        <h2 className="text-3xl text-red-500 drop-shadow tracking-wide">V Olomouci</h2>
        <LocationCard
          name="Taneční centrum při FZŠ Holečkova"
          href="https://www.zsholeckova.cz/"
          mapHref="https://goo.gl/maps/swv3trZB2uvjcQfR6"
          map={{ lat: 49.57963, lng: 17.2495939, zoom: 12 }}
        >
          Holečkova 10, 779 00, Olomouc
          <br />
          (vchod brankou u zastávy Povel - škola)
        </LocationCard>

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

        <h2 className="mt-8 text-3xl text-red-500 drop-shadow tracking-wide">
          V Prostějově
        </h2>
        <LocationCard
          name="Taneční sál Gala"
          mapHref="https://goo.gl/maps/Jtv6mdoSgBEdsiTN7"
          map={{ lat: 49.4681836, lng: 17.0837344, zoom: 12 }}
        >
          Západní 1, 796 04 Prostějov-Krasice
          <br />
          (vchod vedle podnikové prodejny Gala)
        </LocationCard>
      </div>
      <CallToAction />
    </>
  );
}

LocationsPage.getLayout = (page: React.ReactElement) => (
  <Layout showTopMenu>{page}</Layout>
);
