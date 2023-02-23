import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { MyValue, SlateEditor } from 'components/Slate';
import { Layout } from 'components/layout/Layout';
import { Heading } from 'components/Heading';

export default function MembershipPage() {
  return <>
    <Heading
      text="Členství v Olympu"
      image="https://tkolymp.cz/galerie/clanky/289541739553776446369923521898551803548994o.jpg"
      color={{ r: 111, g: 61, b: 1, a: 0.7 }}
    />

    <div className="col-feature my-16 grid gap-16 px-4 md:grid-cols-3">
      <div className="flex items-center justify-center">
        <img src="https://tkolymp.cz/galerie/clanky/293547069655557869525116228763683690860426o.jpg" />
      </div>
      <div className="col-span-2 -order-1 md:order-1">
        <SlateEditor readOnly value={text} />
      </div>
    </div>

    <CallToAction />
  </>;
};

MembershipPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;

const text = [
  {
    type: "p",
    children: [
      {
        text: "Členstvím v TK Olymp se zapojíte do špičkového tanečního klubu:",
        "bold": true
      }
    ]
  },
  {
    type: "ul",
    children: [
      { type: "li", children: [{ text: "Sportovní centrum mládeže - status klubů s nejlepší prací s mládeží v ČR (jediný v Olomouckém kraji)" }] },
      { type: "li", children: [{ text: "Pravidelné tréninky s pěti trenéry Národního reprezentačního týmu" }] },
      { type: "li", children: [{ text: "Moderní tréninkový systém a metodika" }] },
      { type: "li", children: [{ text: "Možnost vstupu na taneční sál 7x týdně pro samostatný trénink" }] },
      { type: "li", children: [{ text: "Zvýhodněná cena tanečních soustředění pořádaných klubem" }] },
      { type: "li", children: [{ text: "Zvýhodněná cena individuálních hodin" }] },
      { type: "li", children: [{ text: "Klubové reprezentační oblečení" }] },
      { type: "li", children: [{ text: "Pravidelné působení nejlepších trenérů z ČR" }] },
      { type: "li", children: [{ text: "Pravidelná spolupráce se zahraničními trenéry" }] },
      { type: "li", children: [{ text: "Spolupráce s mistry světa" }] },
      { type: "li", children: [{ text: "Finanční podpora nejlepších tanečních párů z dotačních titulů" }] },
      { type: "li", children: [{ text: "Pravidelný pořadatel soutěží Taneční ligy" }] },
    ]
  }
] as MyValue
