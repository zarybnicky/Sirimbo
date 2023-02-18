import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { Layout } from 'components/layout/Layout';
import { Heading } from 'components/Heading';
import classNames from 'classnames';

export default function GalleryPage() {
  return <>
    <Heading
      text="Galerie"
      image="https://tkolymp.cz/galerie/clanky/289541739553776446369923521898551803548994o.jpg"
      color={{ r: 111, g: 61, b: 1, a: 0.7 }}
    />

    <div className="col-feature my-16 grid gap-16 px-4 md:grid-cols-2">
      {[{
        image: "/images/services-pro-zacatecniky.png",
        href: "https://www.youtube.com/user/TheMamcro",
        label: <>Videa<br />YouTube</>,
      }, {
        image: "/images/services-soutezni.png",
        href: "https://www.facebook.com/tkolymp/photos_albums?locale=cs_CZ",
        label: <>Fotogalerie<br />Facebook</>,
      }].map(item => (
        <a
          key={item.href}
          target="_blank" rel="noreferrer" href={item.href}
          className="relative aspect-w-16 aspect-h-9 group"
        >
          <div className="absolute inset-0 border-8 border-red-500">
            <img className="w-full h-full object-cover" src={item.image} />
          </div>
          <div className={classNames(
            "absolute inset-0 flex justify-center items-center",
            "text-center text-shadow-2xl text-2xl font-bold text-white",
            "bg-stone-800/50 group-hover:bg-stone-800/70",
          )}>
            {item.label}
          </div>
        </a>
      ))}
    </div>

    <CallToAction />
  </>;
};

GalleryPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;
