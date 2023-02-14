import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { Layout } from 'components/layout/Layout';
import { Heading } from 'components/Heading';

export default function GalleryPage() {
  return <>
    <Heading
      text="Galerie"
      image="https://tkolymp.cz/galerie/clanky/289541739553776446369923521898551803548994o.jpg"
      color={{ r: 111, g: 61, b: 1, a: 0.7 }}
    />

    <div className="col-feature my-16 grid gap-16 px-4 md:grid-cols-2">
      <a target="_blank" rel="noreferrer" href="https://www.facebook.com/tkolymp/photos_albums?locale=cs_CZ">
        Facebook alba fotek
      </a>
      <a target="_blank" rel="noreferrer" href="https://www.youtube.com/user/TheMamcro">
        YouTube videa
      </a>
    </div>

    <CallToAction />
  </>;
};

GalleryPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;
