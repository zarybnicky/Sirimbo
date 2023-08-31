import * as React from 'react';
import { SocialIcons, Sponsors } from '@app/tenant/current/ui';
import { tenantConfig } from '@app/tenant/config.js';
import { ChevronRight } from 'lucide-react';
import { Card } from '@app/ui/Card';
import Link from 'next/link';
import { buttonCls } from '@app/ui/style';

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

        <Link className={buttonCls({ size: 'lg', className: 'mt-4' })} href="/kontakt">
          Kontaktní detaily
          <ChevronRight />
        </Link>

        <div className="my-8">
          <SocialIcons />
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
        <Sponsors />
      </Card>

      <div className="mt-4 col-span-2 flex flex-wrap justify-between">
        <div>{tenantConfig.copyrightLine}</div>
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
