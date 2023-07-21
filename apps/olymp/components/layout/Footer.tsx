import * as React from 'react';
import { Instagram, Facebook, Youtube, ChevronRight } from 'lucide-react';
import { Card } from '@app/ui/Card';

import LogoCsts from 'public/images/logo-csts.svg';
import LogoNsa from 'public/images/logo-nsa.svg';
import LogoProstejov from 'public/images/logo-prostejov.svg';
import LogoOlomouc from 'public/images/logo-olomouc.jpg';
import LogoKraj from 'public/images/logo-kraj.png';
import Link from 'next/link';

const Footer = () => (
  <div className="col-full-width content bg-stone-800 text-white py-12">
    <div className="col-feature grid grid-cols-2 gap-2">
      <h2 className="col-span-2 text-3xl font-bold">Kontakt</h2>
      <div className="col-span-2 md:col-span-1">
        <h3 className="text-lg tracking-wide mt-2 text-red-500 font-bold">
          Taneční klub
        </h3>

        <h4 className="text-lg mt-2 font-black">Taneční klub Olymp Olomouc</h4>

        <div>
          Jiráskova 25, 779 00 Olomouc
          <br />
          IČO: 68347286
          <br />
          miroslav.hyza@tkolymp.cz
        </div>

        <Link className="button button-lg button-accent mt-4" href="/kontakt">
          Kontaktní detaily
          <ChevronRight className="h-3 w-3 ml-2 -mr-2" />
        </Link>

        <div className="my-8 flex gap-2 items-center">
          <a
            target="_blank"
            rel="noreferrer"
            href="https://www.facebook.com/tkolymp"
            className="p-1"
          >
            <Facebook className="text-red-600/90 w-10 h-10" />
          </a>
          <a
            target="_blank"
            rel="noreferrer"
            href="https://www.instagram.com/tanecni_klub_olymp"
            className="p-1"
          >
            <Instagram className="text-red-400 w-10 h-10" />
          </a>
          <a
            target="_blank"
            rel="noreferrer"
            href="https://www.youtube.com/user/TheMamcro"
            className="p-1"
          >
            <Youtube className="text-slate-100 w-10 h-10" />
          </a>
        </div>
      </div>

      <div className="col-span-2 md:col-span-1">
        <h3 className="text-lg tracking-wide mt-2 text-red-500 font-bold">
          Taneční sály
        </h3>

        <h4 className="text-lg mt-2 font-black">Taneční centrum při FZŠ Holečkova</h4>

        <div>
          Holečkova 10, 779 00 Olomouc
          <br />
          (vchod brankou u zastávky Povel, škola)
        </div>

        <br />
        <h4 className="text-lg mt-2 font-black">Tělocvična Slovanského gymnázia</h4>
        <div>
          Jiřího z Poděbrad 13, 779 00 Olomouc
          <br />
          (vchod bránou z ulice U reálky)
        </div>
      </div>

      <Card className="my-8 p-4 col-span-2">
        <h2 className="text-xl text-red-500 font-bold mb-4">Podporují nás</h2>

        <div className="flex flex-wrap lg:flex-nowrap m-4 gap-4 items-stretch justify-center text-center text-stone-500">
          {[
            { label: 'Český svaz tanečního sportu', image: LogoCsts.src },
            { label: 'Město Olomouc', image: LogoOlomouc.src },
            { label: 'Olomoucký kraj', image: LogoKraj.src },
            { label: 'Město Prostějov', image: LogoProstejov.src },
            { label: 'Národní sportovní agentura', image: LogoNsa.src },
          ].map((x) => (
            <div key={x.label} className="flex flex-col grow">
              <div className="grow flex items-center">
                <img alt={x.label} className="w-full h-auto" src={x.image} />
              </div>
              <div className="h-12 mt-4">{x.label}</div>
            </div>
          ))}
        </div>
      </Card>

      <div className="mt-4 col-span-2 flex flex-wrap justify-between">
        <div>© 2023 TK Olymp Olomouc, z. s.</div>
        <div>
          <div>Realizace: Jakub Zárybnický</div>
          <div>
            Verze:{' '}
            {(
              process.env.NEXT_PUBLIC_VERCEL_GIT_COMMIT_SHA || process.env.BUILD_ID
            )?.substring(0, 7)}
          </div>
        </div>
      </div>
    </div>
  </div>
);

const KometaFooter = () => null;

export default process.env.NEXT_PUBLIC_TENANT_ID === '1' ? Footer : KometaFooter;
