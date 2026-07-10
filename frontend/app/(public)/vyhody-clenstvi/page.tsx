/* eslint-disable import-x/no-unused-modules */
import { createPublicPageMetadata } from '@/lib/seo';
import { PageHeader } from '@/ui/TitleBar';
import Image from 'next/image';

export const metadata = createPublicPageMetadata({
  title: 'Členství v Olympu',
  description:
    'Výhody členství v TK Olymp Olomouc: systematické tréninky, špičkoví trenéři, soutěže, soustředění, klubové zázemí a podpora tanečních párů.',
  path: '/vyhody-clenstvi',
});

export default function VyhodyClenstvi() {
  return (
    <>
      <PageHeader title="Členství v Olympu" />

      <div className=" prose prose-accent mb-8">
        <p>
          <b>Členstvím v TK Olymp se zapojíte do špičkového tanečního klubu:</b>
        </p>
        <ul>
          <li>
            Sportovní centrum mládeže - status klubů s nejlepší prací s mládeží v ČR
            (jediný v Olomouckém kraji)
          </li>
          <li>Pravidelné tréninky s pěti trenéry Národního reprezentačního týmu</li>
          <li>Moderní tréninkový systém a metodika</li>
          <li>Možnost vstupu na taneční sál 7x týdně pro samostatný trénink</li>
          <li>Zvýhodněná cena tanečních soustředění pořádaných klubem</li>
          <li>Zvýhodněná cena individuálních hodin</li>
          <li>Klubové reprezentační oblečení</li>
          <li>Pravidelné působení nejlepších trenérů z ČR</li>
          <li>Pravidelná spolupráce se zahraničními trenéry</li>
          <li>Spolupráce s mistry světa</li>
          <li>Finanční podpora nejlepších tanečních párů z dotačních titulů</li>
          <li>Pravidelný pořadatel soutěží Taneční ligy</li>
        </ul>
      </div>
      <div className="col-feature mb-8 ">
        <Image
          alt="Členové TK Olymp Olomouc na mistrovství České republiky družstev"
          src="/images/2022-09-MCRDruzstev.jpg"
          width={5211}
          height={3474}
          sizes="(min-width: 960px) 960px, calc(100vw - 1rem)"
        />
      </div>
    </>
  );
}
