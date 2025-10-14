import Image from 'next/image';
import * as React from 'react';
import { Layout } from '@/components/layout/Layout';
import RoomStandard from './192eb65c0bde40cec93f2ea69d2809ff.webp';
import RoomComfort from './fc2dc9c69b477bcfb63f29f6cc98ae38.webp';
import LogoFloraHotel from './logo flora zelena pantone 7727.svg';
import LogoFlora from './logo-green-download-small.png';
import LogoKraj from '@/tenant/olymp/logo-kraj.png';
import LogoOlomouc from '@/tenant/olymp/logo-olomouc.jpg';
import LogoSoutez from './MCR2025-DRUZSTVA-web-700x300.jpg';

export default function OKlubuPage() {
  return (
    <Layout showTopMenu>
      <Image alt="" src={LogoSoutez} className="mt-3 mb-6" sizes="700px" />
      <div className="prose prose-accent mb-8">
        <h1>Mistrovství ČR v tanečním sportu <br /> Družstva</h1>

        <p>
          Propozice soutěže <a href="https://www.csts.cz/dancesport/kalendar_akci/event/1571/competition/1565/propozice">zde na webu ČSTS</a>
        </p>

        <h2>Ubytování</h2>
        <p>
          Nabídka ubytování v partnerském hotelu pro účastníky MČR Družstev a jejich
          doprovod za zvýhodněnou cenu. Vzdálenost hotelu od haly je 450 m, cca 5 min
          chůze.
        </p>

        <div className="flex flex-wrap gap-4 justify-between items-end">
          <div>
            <h3>Hotel Central Park Flora ****</h3>
            <div className="grid gap-2 grid-cols-3 font-bold">
              <div>Typ pokoje</div> <div><a href="https://www.hotelflora.cz/pokoje/pokoj-comfort">COMFORT</a></div> <div><a href="https://www.hotelflora.cz/pokoje/pokoj-superior">SUPERIOR</a></div>
              <div>jednolůžkový pokoj</div> <div>1800 Kč</div> <div>2100 Kč</div>
              <div>dvoulůžkový pokoj</div> <div>2200 Kč</div> <div>2500 Kč</div>
            </div>
          </div>

          <div className="w-28">
            <Image alt="" src={LogoFloraHotel} className="m-0" sizes="300px" />
          </div>
        </div>

        <p>Cena za pokoj a noc zahrnuje bufetovou snídani a DPH.</p>

        <div className="grid lg:grid-cols-2 gap-4 my-8">
          <div>
            <Image alt="" src={RoomStandard} className="m-0" sizes="300px" />
          </div>
          <div>
            <Image alt="" src={RoomComfort} className="m-0" sizes="300px" />
          </div>
        </div>

        <p>
          Rezervaci lze vytvořit pouze telefonicky na čísle 585 422 200 nebo e-mailem na
          hotelflora@hotelflora.cz pod heslem <b>„MCRDRUZSTVA“</b>. Rezervaci nelze
          provádět přes webové stránky hotelu nebo jiné rezervační systémy.
        </p>
        <p>
          V případě zájmu rezervaci provádějte do 25. 9. 2025. Po tomto termínu již není
          možné zaručit ubytovací kapacitu v hotelu.
        </p>

        <h2>Vstupenky</h2>

        <h3>Předprodej vstupenek</h3>

        <table>
          <tr>
            <td>Místenková vstupenka: (místo u stolu přímo u tanečního parketu)</td>
            <td><b>900 Kč</b></td>
          </tr>
          <tr>
            <td>Nemístenková vstupenka v předprodeji:</td>
            <td><b>600 Kč</b></td>
          </tr>
          <tr>
            <td>Nemístenková vstupenka na místě:</td>
            <td><b>700 Kč</b></td>
          </tr>
          <tr>
            <td>Nemístenková vstupenka na galavečer na místě:</td>
            <td><b>300 Kč</b></td>
          </tr>
          <tr>
            <td>Děti do 15 let:</td>
            <td><b>100 Kč</b></td>
          </tr>
          <tr>
            <td>Senioři 65+:</td>
            <td><b>100 Kč</b></td>
          </tr>
          <tr>
            <td>ZTP:</td>
            <td><b>100 Kč</b></td>
          </tr>
        </table>

        <p>
          Zakoupení místenkových vstupenek ke stolu a nemístenkových vstupenek v předprodeji
          si můžete zajistit podle připraveného plánku stolů
          {' '}
          <a href="https://docs.google.com/spreadsheets/d/1tedgTBZwbzjuRoVfrRkecTlr-2-RRUc5tvTWmH3NC3Y/edit?gid=0#gid=0" target="_blank">ZDE</a>
          {' '}
          na emailu: <b>ticketing@tkolymp.cz</b>
        </p>
        <p>
          Kapacita stolů je omezená - zakoupení míst doporučujeme provést co nejdříve.
        </p>

        <h2>Program akce</h2>
        <p>Brzy bude upřesněno.</p>

        <h2>Partneři akce</h2>

        <div className="flex flex-wrap lg:flex-nowrap m-2 gap-4 items-stretch justify-center text-center text-neutral-11">
          {[
            { label: 'Město Olomouc', image: LogoOlomouc },
            { label: 'Olomoucký kraj', image: LogoKraj },
            { label: 'Výstaviště Flora a.s.', image: LogoFlora },
            { label: 'Central Park Flora', image: LogoFloraHotel },
          ].map((x) => (
            <div key={x.label} className="flex flex-col grow">
              <div className="flex-[1_1_1px] flex items-center justify-end">
                <Image src={x.image} alt={x.label} className="m-0" sizes="100vw" />
              </div>
            </div>
          ))}
        </div>
      </div>
    </Layout>
  );
}
