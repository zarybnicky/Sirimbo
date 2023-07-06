import { CallToAction } from '@app/branding-olymp/CallToAction';
import { Heading } from '@app/ui/Heading';
import { NextSeo } from 'next-seo';
import Image from 'next/image';
import type { NextPageWithLayout } from 'pages/_app';
import VyhodyImage from 'public/images/2022-09-MCRDruzstev.jpg';
import * as React from 'react';

const Page: NextPageWithLayout = () => {
  return (
    <>
      <Heading>Členství v Olympu</Heading>
      <NextSeo title="Členství" />

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
        <Image alt="" src={VyhodyImage} layout="responsive" />
      </div>

      <CallToAction url="/vyhody-clenstvi" />
    </>
  );
};

Page.showTopMenu = true;

export default Page;
