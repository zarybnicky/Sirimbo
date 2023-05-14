import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { Heading } from 'components/Heading';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  return (
    <>
      <Heading>Olymp v tanečním světě</Heading>

      <div className="my-16 col-feature grid gap-16 md:grid-cols-3">
        <div className="flex items-center justify-center">
          <img src="https://tkolymp.cz/galerie/clanky/293547069655557869525116228763683690860426o.jpg" />
        </div>
        <div className="prose col-span-2 -order-1 md:order-1">
          <p>
            Taneční klub byl jedním z prvních klubů v České republice, který v počátku
            devadesátých let 20. století začal systematicky zaměřovat na práci s dětmi a
            mládeží. Jsme klubem s více než třicetiletou tradicí. Zabýváme se výchovou
            tanečních sportovců od dětí až po dospělé. Vytvořili jsme provázaný systém
            téninkových programů pro začínající, výkonnostní i vrcholoví sportovce.
            Využíváme moderní tréninkové metody a pravidelně je zdokonalujeme ve
            spolupráci s odborníky z Fakulty tělesné kultury Univerzity Palackého v
            Olomouci a mezinárodními trenéry World Dance Sport Organisation
          </p>
          <p>
            V rámci Českého svazu tanečního sportu jsme jediným klubem z Olomouckého kraje
            se statutem Sportovního centra mládeže. Pravidelně dodáváme členy národnímu
            reprezentačnímu týmu a dlouhodobě patříme mezi nejlepší české kluby v práci s
            dětmi a mládeží.{' '}
          </p>
          <p>Působíme v Olomouci a Prostějově. </p>
          <p></p>
        </div>
      </div>

      <CallToAction />
    </>
  );
}

Page.showTopMenu = true;

export default Page;
