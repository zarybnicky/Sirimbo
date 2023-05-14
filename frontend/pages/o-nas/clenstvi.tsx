import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { Layout } from 'components/layout/Layout';
import { Heading } from 'components/Heading';

export default function MembershipPage() {
  return (
    <>
      <Heading>Členství v Olympu</Heading>

      <div className="col-feature mt-8 mb-16 grid gap-16 px-4 md:grid-cols-3">
        <div className="flex items-center justify-center">
          <img src="https://tkolymp.cz/galerie/clanky/293547069655557869525116228763683690860426o.jpg" />
        </div>
        <div className="prose col-span-2 -order-1 md:order-1">
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
      </div>

      <CallToAction />
    </>
  );
}

MembershipPage.getLayout = (page: React.ReactElement) => (
  <Layout showTopMenu>{page}</Layout>
);
