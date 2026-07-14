/* eslint-disable import-x/no-unused-modules */
import { publicPageMetadata } from '@/lib/server/seo';
import { PageHeader } from '@/ui/TitleBar';
import Image from 'next/image';
import { Metadata } from 'next';

export const generateMetadata = (): Promise<Metadata> =>
  publicPageMetadata({
    title: 'Olymp v tanečním světě',
    description:
      'Poznejte TK Olymp Olomouc, taneční sportovní klub s více než třicetiletou tradicí, Sportovním centrem mládeže a tréninky v Olomouci a Prostějově.',
    path: '/o-nas',
  });

export default function OKlubuPage() {
  return (
    <>
      <PageHeader title="Olymp v tanečním světě" />

      <div className="prose prose-accent mb-8">
        <p>
          Taneční klub byl jedním z prvních klubů v České republice, který v počátku
          devadesátých let 20. století začal se systematicky zaměřovat na práci s dětmi a
          mládeží. Jsme klubem s více než třicetiletou tradicí. Zabýváme se výchovou
          tanečních sportovců od dětí až po dospělé. Vytvořili jsme provázaný systém
          tréninkových programů pro začínající, výkonnostní i vrcholové sportovce.
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
        <Image
          alt="Tým TK Olymp Olomouc na mistrovství České republiky družstev"
          src="/images/2023-04-MCRDruzstev.jpg"
          width={6373}
          height={3314}
          sizes="(min-width: 960px) 960px, calc(100vw - 1rem)"
        />
      </div>
    </>
  );
}
