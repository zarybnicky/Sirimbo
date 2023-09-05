import { TitleBar } from '@app/ui/TitleBar';
import classNames from 'classnames';
import Image from 'next/image';
import FacebookImage from '@/public/images/Facebook-202209.jpg';
import YoutubeImage from '@/public/images/YouTube-Veverka.jpg';
import * as React from 'react';
import { Layout } from '@/components/layout/Layout';

const Page = () => {
  return (
    <Layout showTopMenu>
      <TitleBar title="Galerie" />

      <div className="col-feature my-16 grid gap-16 px-4 md:grid-cols-2">
        {[
          {
            image: YoutubeImage,
            href: 'https://www.youtube.com/user/TheMamcro',
            label: (
              <>
                Videa
                <br />
                YouTube
              </>
            ),
          },
          {
            image: FacebookImage,
            href: 'https://www.facebook.com/tkolymp/photos_albums?locale=cs_CZ',
            label: (
              <>
                Fotogalerie
                <br />
                Facebook
              </>
            ),
          },
        ].map((item) => (
          <a
            key={item.href}
            target="_blank"
            rel="noreferrer"
            href={item.href}
            className="relative aspect-w-16 aspect-h-9 group"
          >
            <div className="absolute inset-0 border-8 border-red-500 overflow-hidden">
              <Image src={item.image} fill alt="" style={{ objectFit: 'cover'}} />
            </div>
            <div
              className={classNames(
                'absolute inset-0 flex justify-center items-center',
                'text-center text-2xl font-bold text-white',
                'bg-neutral-11/50 group-hover:bg-neutral-11/70',
              )}
            >
              {item.label}
            </div>
          </a>
        ))}
      </div>
    </Layout>
  );
}

export default Page;
