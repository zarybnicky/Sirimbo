import * as React from 'react';
import Banner1 from 'public/images/bannery/TKOLYMP-BANNER-1.png';
import Banner2 from 'public/images/bannery/TKOLYMP-BANNER-2.png';
import Banner3 from 'public/images/bannery/TKOLYMP-BANNER-3.png';
import Banner4 from 'public/images/bannery/TKOLYMP-BANNER-4.png';
import Link from 'next/link';
import { ChevronRight } from 'lucide-react';
import { useRouter } from 'next/router';

const images = [Banner1, Banner2, Banner3, Banner4];

export function CallToAction() {
  const router = useRouter();

  const image = images[Array.from(router.asPath).map((x) => x.charCodeAt(0)).reduce((a,b)=>a+b * 3,0) % 4]!;

  return (
    <div
      className="col-full-width content mt-4 bg-red-500"
      style={{
        backgroundImage: `linear-gradient(90deg, rgba(216,28,58,0.8) 30%, rgba(255,255,255,0) 50%), url(${image.src})`,
        backgroundPosition: 'left, 65% 50%',
        backgroundRepeat: 'no-repeat, no-repeat',
        backgroundSize: '100%, auto 100%',
      }}
    >
      <div className="col-feature px-4 py-16 md:px-8 md:pr-24">
        <div className="text-white font-black text-3xl md:text-4xl">PŘIDEJ SE K NÁM</div>

        <div className="font-bold text-2xl md:text-3xl [text-shadow:0_0_6px_rgba(255,255,255,0.8)]">A OBJEV LÁSKU K TANCI</div>

        <Link href="/prijdtancit" className="mt-4 font-bold tracking-wider button text-white button-lg bg-stone-800 hover:bg-stone-900">
          Chci tančit
          <ChevronRight className="mt-0.5 ml-2 -mr-2" />
        </Link>
      </div>
    </div>
  );
}
