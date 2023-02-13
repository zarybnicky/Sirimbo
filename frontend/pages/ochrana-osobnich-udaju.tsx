import * as React from 'react';
import { Layout } from "components/layout/Layout";
import { SlateEditor } from 'components/Slate';
import { Heading } from 'components/Heading';

export default function TermsConditions() {
  return <>
    <Heading
      text="Ochrana osobních údajů" image="/images/mohelnice2021-title.jpg"
      color={{ r: 216, g: 28, b: 58, a: 0.6 }}
    />
    <SlateEditor readOnly value={text} />
  </>;
}

TermsConditions.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;

const text = [
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": " "
      },
      {
        "text": "I. Základní ustanovení",
        "EMPHASIZE/STRONG": true
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "1. Správcem osobních údajů podle čl. 4 bod 7 nařízení Evropského parlamentu a Rady (EU) 2016/679 o ochraně fyzických osob v souvislosti se zpracováním osobních údajů a o volném pohybu těchto údajů (dále jen: „"
      },
      {
        "text": "GDPR",
        "EMPHASIZE/STRONG": true
      },
      {
        "text": "”) je Taneční klub Olymp Olomou, z. s.., IČ 68347286 se sídlem Jiráskova 25, 77900, Olomouc. (dále jen: „"
      },
      {
        "text": "správce",
        "EMPHASIZE/STRONG": true
      },
      {
        "text": "“)."
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "2. Kontaktní údaje správce jsou"
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "Adresa: Jiráskova 25, 77900, Olomouc"
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "Email: miroslav.hyza@tkolymp.cz"
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "3. Osobními údaji se rozumí veškeré informace o identifikované nebo identifikovatelné fyzické osobě; identifikovatelnou fyzickou osobou je fyzická osoba, kterou lze přímo či nepřímo identifikovat, zejména odkazem na určitý identifikátor, například jméno, identifikační číslo, lokační údaje, síťový identifikátor nebo na jeden či více zvláštních prvků fyzické, fyziologické, genetické, psychické, ekonomické, kulturní nebo společenské identity této fyzické osoby."
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "II. Zdroje a kategorie zpracovávaných osobních údajů",
        "EMPHASIZE/STRONG": true
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "1. Správce zpracovává osobní údaje, které jste mu poskytl/a nebo osobní údaje, které správce získal na základě plnění Vaší objednávky."
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "2. Správce zpracovává Vaše identifikační a kontaktní údaje a údaje nezbytné pro plnění smlouvy."
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "III. Zákonný důvod a účel zpracování osobních údajů",
        "EMPHASIZE/STRONG": true
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "1. Zákonným důvodem zpracování osobních údajů je "
      }
    ]
  },
  {
    "type": "LISTS/UNORDERED-LIST",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "plnění smlouvy mezi Vámi a správcem podle čl. 6 odst. 1 písm. b) GDPR,"
          }
        ]
      },
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "oprávněný zájem správce na poskytování přímého marketingu (zejména pro zasílání obchodních sdělení a newsletterů) podle čl. 6 odst. 1 písm. f) GDPR,"
          }
        ]
      },
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "Váš souhlas se zpracováním pro účely poskytování přímého marketingu (zejména pro zasílání obchodních sdělení a newsletterů) podle čl. 6 odst. 1 písm. a) GDPR ve spojení s § 7 odst. 2 zákona č. 480/2004 Sb., o některých službách informační společnosti v případě, že nedošlo k objednávce zboží nebo služby."
          }
        ]
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": " 2. Účelem zpracování osobních údajů je"
      }
    ]
  },
  {
    "type": "LISTS/UNORDERED-LIST",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "vyřízení Vaší objednávky a výkon práv a povinností vyplývajících ze smluvního vztahu mezi Vámi a správcem; při objednávce jsou vyžadovány osobní údaje, které jsou nutné pro úspěšné vyřízení objednávky (jméno a adresa, kontakt), poskytnutí osobních údajů je nutným požadavkem pro uzavření a plnění smlouvy, bez poskytnutí osobních údajů není možné smlouvu uzavřít či jí ze strany správce plnit,"
          }
        ]
      },
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "zasílání obchodních sdělení a činění dalších marketingových aktivit."
          }
        ]
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": " 3. Ze strany správce dochází k automatickému individuálnímu rozhodování ve smyslu čl. 22 GDPR. S takovým zpracováním jste poskytl/a svůj výslovný souhlas."
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "IV. Doba uchovávání údajů",
        "EMPHASIZE/STRONG": true
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "1. Správce uchovává osobní údaje"
      }
    ]
  },
  {
    "type": "LISTS/UNORDERED-LIST",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "po dobu nezbytnou k výkonu práv a povinností vyplývajících ze smluvního vztahu mezi Vámi a správcem a uplatňování nároků z těchto smluvních vztahů (po dobu 15 let od ukončení smluvního vztahu). "
          }
        ]
      },
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "po dobu, než je odvolán souhlas se zpracováním osobních údajů pro účely marketingu, nejdéle 10 let, jsou-li osobní údaje zpracovávány na základě souhlasu."
          }
        ]
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": " 2. Po uplynutí doby uchovávání osobních údajů správce osobní údaje vymaže."
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "V. Příjemci osobních údajů (subdodavatelé správce)",
        "EMPHASIZE/STRONG": true
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "1. Příjemci osobních údajů jsou osoby "
      }
    ]
  },
  {
    "type": "LISTS/UNORDERED-LIST",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "podílející se na dodání zboží / služeb / realizaci plateb na základě smlouvy, "
          }
        ]
      },
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "zajišťující služby provozování e-shopu (Shoptet) a další služby v souvislosti s provozováním e-shopu,"
          }
        ]
      },
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "zajišťující marketingové služby."
          }
        ]
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": " 2. Správce má v úmyslu předat osobní údaje do třetí země (do země mimo EU) nebo mezinárodní organizaci. Příjemci osobních údajů ve třetích zemích jsou poskytovatelé mailingových služeb a cloudových služeb. "
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "VI. Vaše práva",
        "EMPHASIZE/STRONG": true
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "1. Za podmínek stanovených v GDPR máte "
      }
    ]
  },
  {
    "type": "LISTS/UNORDERED-LIST",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "právo na přístup ke svým osobním údajům dle čl. 15 GDPR, "
          }
        ]
      },
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "právo opravu osobních údajů dle čl. 16 GDPR, popřípadě omezení zpracování dle čl. 18 GDPR. "
          }
        ]
      },
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "právo na výmaz osobních údajů dle čl. 17 GDPR. "
          }
        ]
      },
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "právo vznést námitku proti zpracování dle čl. 21 GDPR a "
          }
        ]
      },
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "právo na přenositelnost údajů dle čl. 20 GDPR. "
          }
        ]
      },
      {
        "type": "LISTS/LIST-ITEM",
        "children": [
          {
            "text": "právo odvolat souhlas se zpracováním písemně nebo elektronicky na adresu nebo email správce uvedený v čl. III těchto podmínek."
          }
        ]
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": " 2. Dále máte právo podat stížnost u Úřadu pro ochranu osobních údajů v případě, že se domníváte, že bylo porušeno Vaší právo na ochranu osobních údajů."
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "VII. Podmínky zabezpečení osobních údajů",
        "EMPHASIZE/STRONG": true
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "1. Správce prohlašuje, že přijal veškerá vhodná technická a organizační opatření k zabezpečení osobních údajů."
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "2. Správce přijal technická opatření k zabezpečení datových úložišť a úložišť osobních údajů v listinné podobě."
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "3. Správce prohlašuje, že k osobním údajům mají přístup pouze jím pověřené osoby."
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "VIII. Závěrečná ustanovení",
        "EMPHASIZE/STRONG": true
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "1. Odesláním objednávky z internetového objednávkového formuláře potvrzujete, že jste seznámen/a s podmínkami ochrany osobních údajů a že je v celém rozsahu přijímáte."
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "2. S těmito podmínkami souhlasíte zaškrtnutím souhlasu prostřednictvím internetového formuláře. Zaškrtnutím souhlasu potvrzujete, že jste seznámen/a s podmínkami ochrany osobních údajů a že je v celém rozsahu přijímáte."
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "3. Správce je oprávněn tyto podmínky změnit. Novou verzi podmínek ochrany osobních údajů zveřejní na svých internetových stránkách a zároveň Vám zašle novou verzi těchto podmínek Vaši e-mailovou adresu, kterou jste správci poskytl/a."
      }
    ]
  },
  {
    "type": "PARAGRAPH/PARAGRAPH",
    "data": {
      "align": "start"
    },
    "children": [
      {
        "text": "Tyto podmínky nabývají účinnosti dnem 1.9.2020."
      }
    ]
  }
];
