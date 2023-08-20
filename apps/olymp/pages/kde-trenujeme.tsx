import { TitleBar } from '@app/ui/TitleBar';
import { LocationCard } from '@app/ui/cards/LocationCard';
import * as React from 'react';
import Image from 'next/image';
import { Layout } from 'components/layout/Layout';

const Page = () => {
  return (
    <Layout showTopMenu>
      <TitleBar title="Kde trénujeme" />

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
          (vchod brankou u zastávy Povel - škola)
        </LocationCard>

        <div className="min-h-[200px] md:min-h-[300px] my-4 relative">
          <Image
            className="object-cover"
            alt=""
            src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687512915639-Saly-Holeckova.jpg"
            fill
            sizes=""
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
            alt=""
            src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687512915639-Saly-SGO.jpg"
            fill
            sizes=""
          />
        </div>
      </div>
    </Layout>
  );
}

export default Page;
