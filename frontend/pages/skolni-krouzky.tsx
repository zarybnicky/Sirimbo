import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { Heading } from 'components/Heading';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  return (
    <>
      <Heading>Olymp Dance</Heading>

      <div className="mt-6 prose">
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
          V tuto chvíli působíme na několika Základních školách v okolí Olomouce a
          Prostějova.
        </p>
      </div>

      <div className="flex justify-center">
        <a
          className="button button-red button-lg hover:underline my-6"
          href="https://olympdance.cz"
          target="blank"
          rel="nofollow"
        >
          Najdi si školu, kde se tančí
        </a>
      </div>

      <img className="my-8" src="https://tkolymp.cz/galerie/clanky/Týmové-foto-1.jpg" />

      <CallToAction />
    </>
  );
}

Page.staticTitle = "Kroužky";
Page.showTopMenu = true;

export default Page;
