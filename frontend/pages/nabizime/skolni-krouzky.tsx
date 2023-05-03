import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { Layout } from 'components/layout/Layout';
import { Heading } from 'components/Heading';
import { YoutubeEmbed } from 'components/YoutubeEmbed';

export default function ExtracurricularsPage() {
  return (
    <>
      <Heading
        text="OLYMP DANCE"
        image="https://tkolymp.cz/galerie/clanky/prijdtancit2.jpg"
        color={{ r: 216, g: 28, b: 58, a: 0.4 }}
      />

      <div className="mt-6 prose">
        <h1>Tanči s námi na své škole!</h1>

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

      <img src="https://tkolymp.cz/galerie/clanky/Týmové-foto-1.jpg" />

      <CallToAction />
    </>
  );
}

ExtracurricularsPage.getLayout = (page: React.ReactElement) => (
  <Layout showTopMenu>{page}</Layout>
);
