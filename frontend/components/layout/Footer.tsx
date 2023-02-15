import * as React from 'react';
import { SocialButtons } from './SocialButtons';
import { Map } from 'components/Map';
import { Card } from 'components/Card';

import LogoCsts from 'public/images/logo-csts.svg';
import LogoNsa from 'public/images/logo-nsa.svg';
import LogoProstejov from 'public/images/logo-prostejov.svg';
import LogoOlomouc from 'public/images/logo-olomouc.jpg';
import LogoKraj from 'public/images/logo-kraj.png';

export const FooterMap = ({ height = '200px' }) => {
  const position = [49.58727525, 17.25661055] as [number, number];
  const holeckova = [49.57963, 17.2495939] as [number, number];
  const slovan = [49.59490, 17.26340] as [number, number];

  return <Map center={position} zoom={12} scrollWheelZoom={false} style={{ height }}>
    {({ TileLayer, Marker, Popup }) => <>
      <TileLayer url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" />
      <Marker position={holeckova}>
        <Popup>Taneční centrum při FZŠ Holečkova</Popup>
      </Marker>
      <Marker position={slovan}>
        <Popup>Tělocvična Slovanského gymnázia</Popup>
      </Marker>
    </>}
  </Map>;
};

export const Footer: React.FC = () => {
  return <div className="col-full-width content bg-stone-800 text-white pt-12 pb-20">
    <div className="col-feature grid grid-cols-1 md:grid-cols-2 gap-2">
      <div>
        <h2 className="text-3xl font-bold">Kontakt:</h2>

        <h3 className="text-lg tracking-wide mt-4 text-red-500 font-bold">Taneční klub</h3>

        <h4 className="text-lg mt-4 font-black">Taneční klub Olymp Olomouc</h4>

        <div>
          Jiráskova 25, 779 00 Olomouc<br />
          IČO: 68347286<br />
          miroslav.hyza@tkolymp.cz
        </div>

        <br />
        <br />

        <h3 className="text-lg tracking-wide mt-2 text-red-500 font-bold">Taneční sály</h3>

        <h4 className="text-lg mt-2 font-black">Taneční centrum při FZŠ Holečkova</h4>

        <div>
          Holečkova 10, 779 00 Olomouc<br />
          (vchod brankou u zastávky Povel, škola)
        </div>

        <br />
        <h4 className="text-lg mt-2 font-black">Tělocvična Slovanského gymnázia</h4>
        <div>
          Jiřího z Poděbrad 13, 779 00 Olomouc<br />
          (vchod bránou z ulice U reálky)
        </div>

        <div className="mt-12">Verze: {process.env.BUILD_ID?.substring(0, 7)}</div>
        <div>Realizace: Jakub Zárybnický</div>
        <div>© 2023 Taneční klub Olymp Olomouc, z. s.</div>
        <div>Verze: {process.env.BUILD_ID?.substring(0, 7)}</div>
      </div>

      <div>
        <FooterMap />
        <div className="flex justify-end my-8">
          <SocialButtons variant="large" />
        </div>
      </div>

      <Card className="p-4 col-span-2">
        <h2 className="text-xl text-red-500 font-bold mb-4">Podporují nás</h2>

        <div className="flex flex-wrap lg:flex-nowrap m-4 gap-4 items-end justify-center text-center text-stone-500">
          <div>
            <img alt="Český svaz tanečního sportu" style={{ width: '100%', height: 'auto' }} src={LogoCsts.src} />
            <div className="h-12 mt-4">Český svaz tanečního sportu</div>
          </div>
          <div>
            <img alt="Město Olomouc" style={{ width: '100%', height: 'auto' }} src={LogoOlomouc.src} />
            <div className="h-12 mt-4">Město Olomouc</div>
          </div>
          <div>
            <img alt="Olomoucký kraj" style={{ width: '100%', height: 'auto' }} src={LogoKraj.src} />
            <div className="h-12 mt-4">Olomoucký kraj</div>
          </div>
          <div>
            <img alt="Město Prostějov" style={{ width: '100%', height: 'auto' }} src={LogoProstejov.src} />
            <div className="h-12 mt-4">Město Prostějov</div>
          </div>
          <div>
            <img alt="Národní sportovní agentura" style={{ width: '100%', height: 'auto' }} src={LogoNsa.src} />
            <div className="h-12 mt-4">Národní sportovní agentura</div>
          </div>
        </div>
      </Card>
    </div>
  </div>;
};
