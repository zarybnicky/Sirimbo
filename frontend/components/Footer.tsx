import * as React from 'react';
import { SocialButtons } from './SocialButtons';
import { Map } from './Map';
import { Card } from './Card';

import CstsLogo from 'public/images/csts-logo.svg';
import OlomoucLogo from 'public/style/logo-olomouc.jpg';
import KrajLogo from 'public/style/logo-kraj.png';

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
  return <div className="bg-stone-800 text-white pt-12 pb-20">
    <div className="container mx-auto max-w-5xl">
      <div className="grid grid-cols-1 md:grid-cols-2 gap-2">
        <div>
          <h2 className="text-2xl font-bold">Kontakt:</h2>

          <h3 className="text-lg tracking-wide mt-4 text-red-500 font-bold">Taneční klub</h3>

          <h4 className="text-lg tracking-wide mt-4 font-black">Taneční klub Olymp Olomouc</h4>

          <div>
            Jiráskova 25, 779 00 Olomouc<br />
            IČO: 68347286<br />
            miroslav.hyza@tkolymp.cz
          </div>

          <br />
          <br />

          <h3 className="text-lg tracking-wide mt-2 text-red-500 font-bold">Taneční sály</h3>

          <h4 className="text-lg tracking-wide mt-2 font-black">Taneční centrum při FZŠ Holečkova</h4>

          <div>
            Holečkova 10, 779 00 Olomouc<br />
            (vchod brankou u zastávky Povel, škola)
          </div>

          <br />
          <h4 className="text-lg tracking-wide mt-2 font-black">Tělocvična Slovanského gymnázia</h4>
          <div>
            Jiřího z Poděbrad 13, 779 00 Olomouc<br />
            (vchod bránou z ulice U reálky)
          </div>

          <div className="mt-12">Realizace: Jakub Zárybnický</div>
          <div>© 2022 Taneční klub Olymp Olomouc, z. s.</div>
        </div>

        <div>
          <FooterMap />
          <SocialButtons variant="large" className="text-right my-4" />

          <Card className="p-4">
            <h2 className="text-xl text-red-500 font-bold mb-4">Podporují nás</h2>

            <div className="grid grid-cols-2 gap-2 text-center">
              <div className="row-span-2">
                <img alt="ČSTS" style={{ width: '100%', height: 'auto' }} src={CstsLogo.src} />
              </div>
              <div>
                <img alt="Olomoucký kraj" style={{ width: '100%', height: 'auto' }} src={KrajLogo.src} />
              </div>
              <div>
                <img alt="Město Olomouc" style={{ width: '100%', height: 'auto' }} src={OlomoucLogo.src} />
              </div>
            </div>
          </Card>
        </div>
      </div>
    </div>
  </div>;
};
