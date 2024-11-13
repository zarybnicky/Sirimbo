import { TitleBar } from '@/ui/TitleBar';
import Image from 'next/image';
import * as React from 'react';
import { Layout } from '@/components/layout/Layout';
import { buttonCls } from '@/ui/style';

export default function SkolniKrouzkyPage() {
  return (
    <Layout showTopMenu>
      <TitleBar title="Olymp Dance" />

      <div className="mt-6 prose prose-accent">
        <h2>Tanči s námi na své škole!</h2>

        <p>
          Projekt Taneční kroužky Olymp DANCE vznikl v roce 2019 za hlavním účelem
          přiblížit dětem možnost tanečních tréninků na jejich škole. Děti nemusí nikam
          dojíždět, ale lektor dojíždí za nimi.
        </p>

        <p>
          Jednou za pololetí je čeká Akademie formou vystoupení pro veřejnost. Děti z
          jednotlivých škol se představí se svými choreografiemi na moderní tanečky + se
          naučí základy latinskoamerických a standardních tanců.
        </p>

        <p>
          Během roku pořádáme několik tanečních akcí. Jsou to například jednodenní
          soustředění, Vánoční večírek nebo v létě pobytové taneční campy.
        </p>

        <p>
          V tuto chvíli působíme na několika základních školách v okolí Olomouce a
          Prostějova.
        </p>
      </div>

      <div className="flex justify-center">
        <a
          className={buttonCls({ size: 'lg', className: 'my-6' })}
          href="https://olympdance.cz"
          target="blank"
          rel="nofollow"
        >
          Najdi si školu, kde se tančí
        </a>
      </div>

      <div className="my-8 relative min-h-[50vh]">
        <Image
          className="object-contain"
          src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687512915638-OlympDance-uvodni-foto.jpg"
          alt="Společné foto závěrečného vystoupení Olymp Dance"
          sizes="(max-width: 768px) 100vw, 50vw"
          fill
        />
      </div>
    </Layout>
  );
}
