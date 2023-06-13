import * as React from 'react';
import { LocationCard } from 'components/cards/LocationCard';
import { CallToAction } from 'components/CallToAction';
import { Heading } from 'components/Heading';
import type { NextPageWithLayout } from 'pages/_app';
import { NextSeo } from 'next-seo';

const Page: NextPageWithLayout = () => {
  return (
    <>
      <Heading>Kde trénujeme</Heading>
      <NextSeo title="Kde trénujeme" />

      <div className="my-16 space-y-4">
        <h2 className="text-3xl text-accent-11 drop-shadow tracking-wide">V Olomouci</h2>
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
      </div>
      <CallToAction />
    </>
  );
}

Page.showTopMenu = true;

export default Page;
