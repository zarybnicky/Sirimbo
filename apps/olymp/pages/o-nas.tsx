import { CallToAction } from '@app/branding-olymp/CallToAction';
import { Heading } from '@app/ui/Heading';
import Image from 'next/image';
import type { NextPageWithLayout } from 'pages/_app';
import ONasImage from 'public/images/2023-04-MCRDruzstev.jpg';
import * as React from 'react';

const Page: NextPageWithLayout = () => {
  return (
    <>
      <Heading>Olymp v tanečním světě</Heading>

      <div className="prose prose-accent mb-8">
        <p>
          Taneční klub byl jedním z prvních klubů v České republice, který v počátku
          devadesátých let 20. století začal systematicky zaměřovat na práci s dětmi a
          mládeží. Jsme klubem s více než třicetiletou tradicí. Zabýváme se výchovou
          tanečních sportovců od dětí až po dospělé. Vytvořili jsme provázaný systém
          téninkových programů pro začínající, výkonnostní i vrcholoví sportovce.
          Využíváme moderní tréninkové metody a pravidelně je zdokonalujeme ve spolupráci
          s odborníky z Fakulty tělesné kultury Univerzity Palackého v Olomouci a
          mezinárodními trenéry World Dance Sport Federation.
        </p>
        <p>
          V rámci Českého svazu tanečního sportu jsme jediným klubem z Olomouckého kraje
          se statutem Sportovního centra mládeže. Pravidelně dodáváme členy národnímu
          reprezentačnímu týmu a dlouhodobě patříme mezi nejlepší české kluby v práci s
          dětmi a mládeží.
        </p>
        <p>Působíme v Olomouci a Prostějově. </p>
      </div>
      <div className="col-feature mb-16">
        <Image alt="" src={ONasImage} layout="responsive" sizes="1256px" />
      </div>

      <CallToAction />
    </>
  );
};

Page.showTopMenu = true;
Page.staticTitle = 'O klubu';

export default Page;
