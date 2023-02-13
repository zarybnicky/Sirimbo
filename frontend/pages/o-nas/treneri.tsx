import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { SlateEditor } from 'components/Slate';
import { Layout } from 'components/layout/Layout';
import { Heading } from 'components/Heading';
import { TrainerCard } from 'components/cards/TrainerCard';
import { Descendant } from 'slate';

export default function AboutPage() {
  return <>
    <Heading
      text="Trenéři"
      image="https://tkolymp.cz/galerie/clanky/289541739553776446369923521898551803548994o.jpg"
      color={{ r: 111, g: 61, b: 1, a: 0.7 }}
    />

    <div className="col-feature mt-16 prose">
      <h2>Kluboví trenéři</h2>
    </div>

    <div className="col-feature my-4 grid gap-8 lg:grid-cols-2">
      {internal.map(item => (
        <TrainerCard key={item.name} name={item.name} image={item.image}>
          <SlateEditor readOnly value={item.content} />
        </TrainerCard>
      ))}
    </div>

    <div className="col-feature mt-16 prose">
      <h2>Externí trenéři</h2>
    </div>

    <div className="col-feature mt-4 mb-16 grid gap-8 lg:grid-cols-2">
      {internal.map(item => (
        <TrainerCard key={item.name} name={item.name} image={item.image}>
          <SlateEditor readOnly value={item.content} />
        </TrainerCard>
      ))}
    </div>

    <CallToAction />
  </>;
};

AboutPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;

const mirek = {
  "name": "Mgr. Miroslav Hýža",
  "image": "/images/services-pripravka.png",
  content: [
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
              "text": "Předseda, šéftrenér TK Olymp"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Trenér mistrů ČR"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Vicemistr ČR ve standardních tancích v hlavní kategorii 2018"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "3x mistr ČR v kategorii juniorů"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Finalista Akademického mistrovství Evropy 2016"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Držitel ocenění TOP 30 sportovních trenérů mládeže 2017"
            }
          ]
        }
      ]
    }
  ],
};

const filip = {
  "name": "Ing. Filip Karásek",
  "image": "/images/treneri/filip.jpg",
  content: [
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
              "text": "Trenér a porotce II. třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "2x mistr ČR v latinsko-amerických tancích v kategorii profesionálů"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "3x mistr ČR v latinsko-amerických tancích v hlavní kategorii"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Semifinalista mistrovství světa a Evropy v kategorii profesionálů"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Semifinalista mistrovství Evropy v hlavní kategorii"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Čtvrtfinalista mistrovství světa v hlavní kategorii"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Finalista GOC ve Stuttgartu kategorii profesionálů"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Semifinalista Světových her v Kolumbii"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Předseda Komise sportovců (or r. 2017)"
            }
          ]
        }
      ]
    }
  ]
};

const marie = {
  "name": "Mgr. Marie Hýžová",
  "image": "/images/treneri/marie2.jpg",
  content: [
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
              "text": "Trenér a porotce I. třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Dlouholetá tanečnice mezinárodní třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Zkušená trenérka dětských a juniorských přípravek"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Švadlena tanečního oblečení (řadu let šatí finalisty i medailisty mistrovství ČR)"
            }
          ]
        }
      ]
    }
  ]
};

const lucka = {
  "name": "Mgr. Lucie Benýšková",
  "image": "/images/treneri/lucka3.jpg",
  content: [
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
              "text": "Absolventka kvalifikačního studia pro trenéry a porotce II.třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Semifinalistka mistrovství ČR v hlavní kategorii (v deseti tancích a ve standardních tancích)"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "3x finalistka mistrovství ČR juniorů a mládeže (titul druhý vicemistr ČR ve standardních tancích v kategorii mládež)"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "2. místo v Taneční lize (žebříček párů mezinárodní třídy)"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Trenérka dětských a juniorských přípravek (12 let praxe)"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Trenérka finalistů mistrovství ČR juniorů"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Absolventka Fakulty tělesné kultury na UP v Olomouci (obor: Tělesná výchova a sport)"
            }
          ]
        }
      ]
    }
  ]
};

const grepi = {
  "name": "Mgr. Pavel Grepl",
  "image": "/images/treneri/pavel2.jpg",
  content: [
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
              "text": "Absolvent kvalifikačního studia pro trenéry a porotce II. třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Tanečník mezinárodní třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Finalista Akademického mistrovství ČR ve standardních tancích"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Student Fakulty tělesné kultury na UP v Olomouci (obor: Trenérství a sport)"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Trenér atletiky III. třídy a plavání III. třídy"
            }
          ]
        }
      ]
    }
  ]
};

const maruska = {
  "name": "Bc. Marie Hýžová ml.",
  "image": "/images/treneri/maruska.jpg",
  content: [
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
              "text": "Absolventka kvalifikačního studia pro trenéry a porotce II. třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Finalistka mistrovství ČR v deseti tancích"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Semifinalistka mistrovství ČR ve standardních tancích"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Čtvrtfinalistka mistrovství ČR v latinskoamerických tancích"
            }
          ]
        }
      ]
    }
  ]
};

const roman = {
  "name": "Bc. Roman Pecha",
  content: [
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
              "text": "Absolvent kvalifikačního studia pro trenéry a porotce II. třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Tanečník mezinárodní třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Finalista mistrovství ČR v hlavní kategorii (ve standardních tancích)"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "2x mistr ČR v kategorii U21 (v deseti tancích a ve standardních tancích)"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Čtvrtfinalista mistrovství světa a Evropy v kategorii mládež ve standardních tancích"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Čtvrtfinalista mistrovství světa v kategorii mládež a U21 v deseti tancích"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Student MVŠO"
            }
          ]
        }
      ]
    }
  ]
};

const hanka = {
  "name": "Hana Anna Šišková",
  content: [
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
              "text": "Absolventka kvalifikačního studia pro trenéry a porotce III. třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Tanečnice mezinárodní třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "4x finalistka mistrovství ČR v kategorii U21 (titul vicemistr ČR v deseti tancích, titul druhý vicemistr ČR ve standardních tancích a titul druhý vicemistr ČR v latinskoamerických tancích)"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Studentka Fakulty tělesné kultury na UP v Olomouci (obor: Tělesná výchova a sport)"
            }
          ]
        }
      ]
    }
  ]
};

const nela = {
  "name": "Nela Šírová",
  content: [
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
              "text": "Absolventka kvalifikačního studia pro trenéry a porotce III. třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Tanečnice mezinárodní třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "3x finalistka mistrovství ČR juniorů a mládeže"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Studentka Lékařské fakulty na UP v Olomouci"
            }
          ]
        }
      ]
    }
  ]
};

const matej = {
  "name": "Matěj Očenášek",
  "image": "/images/treneri/matej.jpg",
  content: [
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
              "text": "Absolvent kvalifikačního studia pro trenéry a porotce III. třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "2x finalista mistrovství ČR mládeže"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "3x finalista mistrovství ČR v kategorii U21 (vicemistr ČR v deseti tancích)"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Student VUT FEKT"
            }
          ]
        }
      ]
    }
  ]
};

const martin = {
  "name": "Martin Odstrčil",
  "image": "/images/treneri/martin.jpg",
  content: [
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
              "text": "Prezident DSP Kometa Brno"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Trenér finalistů mistrovství světa a medailistů mistrovství Evropy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Trenér mistrů České republiky všech věkových kategorií"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "6x mistr ČR v deseti tancích (1995-2000)"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Mistr ČR ve standardních tancích (2000)"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Trenér a porotce I. třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Porotce s nejvyšší mezinárodní licencí WDSF"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Porotce mistrovství světa a Evropy"
            }
          ]
        }
      ]
    }
  ]
};

const pavla = {
  "name": "Pavla Landsfeldová",
  "image": "/images/treneri/pavla.jpg",
  content: [
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
              "text": "Trenérka mistrů České republiky ve standardních tancích"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Finalistka mistrovství ČSSR ve standardních tancích"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Mistryně ČR ve standardních tancích v kategorii senior (1996)"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Trenérka a porotkyně I. třídy"
            }
          ]
        }
      ]
    }
  ]
};

const jerry = {
  "name": "Ing. Jaroslav Kučera",
  "image": "/images/treneri/jerry.jpg",
  content: [
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
              "text": "Předseda trenérské rady projektu Sportovně talentované mládeže ČSTS"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Trenér finalistů mistrovství světa a medailistů mistrovství Evropy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Trenér mistrů České republiky všech věkových kategorií"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Vicemistr ČR v latinskoamerických tancích (1992, 1993, 1995)"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Finalista mistrovství ČR v deseti tancích (1993)"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Trenér a porotce i. třídy"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Porotce s nejvyšší mezinárodní licencí WDSF"
            }
          ]
        },
        {
          "type": "LISTS/LIST-ITEM",
          "children": [
            {
              "text": "Porotce mistrovství Evropy"
            }
          ]
        }
      ]
    }
  ]
};

const internal: {
  name: string; image?: string; content: Descendant[];
}[] = [mirek, filip, marie, lucka, grepi, maruska, roman, hanka, nela, matej];

const external: {
  name: string; image?: string; content: Descendant[];
}[] = [martin, pavla, jerry];
