import * as React from 'react';
import type { CellPlugin } from '@react-page/editor';
import CtaImage from 'public/images/call-to-action.png';
import Link from 'next/link';

export const CallToAction = ({ }) => {
  return <div className="bg-red-500">
    <div className="container mx-auto max-w-5xl flex justify-left" style={{
      backgroundImage: `url(${CtaImage.src})`,
      backgroundPosition: '85% 50%',
      backgroundRepeat: 'no-repeat',
      backgroundSize: 'auto 100%',
    }}>
      <div className="px-4 py-8 md:p-16 md:pr-24" style={{
        background: 'linear-gradient(90deg, rgba(216,28,58,0.6) 70%, rgba(0,0,0,0) 100%)',
      }}>
        <div className="text-white font-black text-3xl md:text-4xl">
          PŘIDEJ SE K NÁM
        </div>

        <div className="font-bold text-2xl md:text-3xl">
          A OBJEV LÁSKU K TANCI
        </div>

        <Link href="/treninkove-programy" passHref>
          <a className="button button-grey bg-stone-800" style={{
            textTransform: 'none',
            marginTop: '1.25rem',
            padding: '.75rem 3rem',
            borderRadius: 0,
            fontSize: '140%',
          }}>Chci tančit</a>
        </Link>
      </div>
    </div>
  </div>;
};

export const CallToActionPlugin: CellPlugin<{}> = {
  Renderer: () => <CallToAction />,

  id: 'app-cta-plugin',
  title: 'Call to Action',
  description: undefined,
  version: 1,
};
