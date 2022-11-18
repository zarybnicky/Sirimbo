import * as React from 'react';
import type { CellPlugin } from '@react-page/editor';
import CtaImage from 'public/images/call-to-action.png';
import Link from 'next/link';
import { ChevronRight } from 'react-feather';

export function CallToAction() {
  return <div className="col-full-width content mt-4 bg-red-500" style={{
    backgroundImage: `linear-gradient(90deg, rgba(216,28,58,0.6) 30%, rgba(0,0,0,0) 70%), url(${CtaImage.src})`,
    backgroundPosition: 'left, 85% 50%',
    backgroundRepeat: 'no-repeat, no-repeat',
    backgroundSize: '100%, auto 100%',
  }}>
    <div className="col-feature px-4 py-10 md:p-16 md:pr-24 drop-shadow-xl">
      <div className="text-white font-black text-3xl md:text-4xl">
        PŘIDEJ SE K NÁM
      </div>

      <div className="font-bold text-2xl md:text-3xl">
        A OBJEV LÁSKU K TANCI
      </div>

      <Link href="/prijdtancit" passHref>
        <a className="mt-4 font-bold tracking-wider button text-white button-lg bg-stone-800 hover:bg-stone-900">
          Chci tančit
          <ChevronRight className="mt-0.5 ml-2 -mr-2" />
        </a>
      </Link>
    </div>
  </div>;
}

export const CallToActionPlugin: CellPlugin<{}> = {
  Renderer: () => <CallToAction />,

  id: 'app-cta-plugin',
  title: 'Call to Action',
  description: undefined,
  version: 1,
};
