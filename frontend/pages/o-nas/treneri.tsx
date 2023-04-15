import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { MyValue, SlateEditor } from 'components/Slate';
import { Layout } from 'components/layout/Layout';
import { Heading } from 'components/Heading';
import { TrainerCard } from 'components/cards/TrainerCard';

export default function AboutPage() {
  return (
    <>
      <Heading
        text="Trenéři"
        image="https://tkolymp.cz/galerie/clanky/289541739553776446369923521898551803548994o.jpg"
        color={{ r: 111, g: 61, b: 1, a: 0.7 }}
      />

      <div className="col-feature mt-16 prose">
        <h2>Kluboví trenéři</h2>
      </div>

      <div className="col-feature my-4 grid gap-8 lg:grid-cols-2">
        {internal.map((item) => (
          <TrainerCard key={item.name} name={item.name} image={item.image}>
            <SlateEditor readOnly value={item.content} />
          </TrainerCard>
        ))}
      </div>

      <div className="col-feature mt-16 prose">
        <h2>Externí trenéři</h2>
      </div>

      <div className="col-feature mt-4 mb-16 grid gap-8 lg:grid-cols-2">
        {external.map((item) => (
          <TrainerCard key={item.name} name={item.name} image={item.image}>
            <SlateEditor readOnly value={item.content} />
          </TrainerCard>
        ))}
      </div>

      <CallToAction />
    </>
  );
}

AboutPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;

const mirek = {
  name: 'Mgr. Miroslav Hýža',
  image: '/images/services-pripravka.png',
  content: [
    {
      type: 'ul',
      data: {
        align: 'start',
      },
      children: [
        {
          type: 'li',
          children: [{ text: 'Předseda, šéftrenér TK Olymp' }],
        },
        {
          type: 'li',
          children: [{ text: 'Trenér mistrů ČR' }],
        },
        {
          type: 'li',
          children: [
            { text: 'Vicemistr ČR ve standardních tancích v hlavní kategorii 2018' },
          ],
        },
        {
          type: 'li',
          children: [{ text: '3x mistr ČR v kategorii juniorů' }],
        },
        {
          type: 'li',
          children: [{ text: 'Finalista Akademického mistrovství Evropy 2016' }],
        },
        {
          type: 'li',
          children: [{ text: 'Držitel ocenění TOP 30 sportovních trenérů mládeže 2017' }],
        },
      ],
    },
  ] as MyValue,
};

const filip = {
  name: 'Ing. Filip Karásek',
  image: '/images/treneri/filip.jpg',
  content: [
    {
      type: 'ul',
      data: {
        align: 'start',
      },
      children: [
        {
          type: 'li',
          children: [{ text: 'Trenér a porotce I. třídy' }],
        },
        {
          type: 'li',
          children: [
            { text: '3x mistr ČR v latinsko-amerických tancích v kategorii Dopspělí' },
          ],
        },
        {
          type: 'li',
          children: [
            {
              text: '2x mistr ČR v latinsko-amerických tancích v kategorii Profesionálů',
            },
          ],
        },
        {
          type: 'li',
          children: [
            { text: 'Čtvrtfinalista mistrovství světa a Evropy v kategorii Dospělí' },
          ],
        },
        {
          type: 'li',
          children: [
            {
              text: 'Semifinalista mistrovství světa a Evropy v kategorii Profesionálové',
            },
          ],
        },
        {
          type: 'li',
          children: [
            {
              text: 'Finalista German Open Championship (Stuttgart) v kategorii Profesionálové',
            },
          ],
        },
        {
          type: 'li',
          children: [{ text: 'Semifinalista Světových her v Kolumbii v roce 2013' }],
        },
        {
          type: 'li',
          children: [{ text: 'Předseda Komise sportovců ČSTS (2017-2019)' }],
        },
        {
          type: 'li',
          children: [{ text: 'Viceprezident ČSTS (od roku 2019)' }],
        },
      ],
    },
  ] as MyValue,
};

const marie = {
  name: 'Mgr. Marie Hýžová',
  image: '/images/treneri/marie2.jpg',
  content: [
    {
      type: 'ul',
      data: {
        align: 'start',
      },
      children: [
        {
          type: 'li',
          children: [{ text: 'Trenér a porotce I. třídy' }],
        },
        {
          type: 'li',
          children: [{ text: 'Dlouholetá tanečnice mezinárodní třídy' }],
        },
        {
          type: 'li',
          children: [{ text: 'Zkušená trenérka dětských a juniorských přípravek' }],
        },
        {
          type: 'li',
          children: [
            {
              text: 'Švadlena tanečního oblečení (řadu let šatí finalisty i medailisty mistrovství ČR)',
            },
          ],
        },
      ],
    },
  ] as MyValue,
};

const lucka = {
  name: 'Mgr. Lucie Benýšková',
  image: '/images/treneri/lucka3.jpg',
  content: [
    {
      type: 'ul',
      data: {
        align: 'start',
      },
      children: [
        {
          type: 'li',
          children: [
            { text: 'Absolventka kvalifikačního studia pro trenéry a porotce II.třídy' },
          ],
        },
        {
          type: 'li',
          children: [
            {
              text: 'Semifinalistka mistrovství ČR v hlavní kategorii (v deseti tancích a ve standardních tancích)',
            },
          ],
        },
        {
          type: 'li',
          children: [
            {
              text: '3x finalistka mistrovství ČR juniorů a mládeže (titul druhý vicemistr ČR ve standardních tancích v kategorii mládež)',
            },
          ],
        },
        {
          type: 'li',
          children: [
            { text: '2. místo v Taneční lize (žebříček párů mezinárodní třídy)' },
          ],
        },
        {
          type: 'li',
          children: [
            { text: 'Trenérka dětských a juniorských přípravek (12 let praxe)' },
          ],
        },
        {
          type: 'li',
          children: [{ text: 'Trenérka finalistů mistrovství ČR juniorů' }],
        },
        {
          type: 'li',
          children: [
            {
              text: 'Absolventka Fakulty tělesné kultury na UP v Olomouci (obor: Tělesná výchova a sport)',
            },
          ],
        },
      ],
    },
  ] as MyValue,
};

const grepi = {
  name: 'Mgr. Pavel Grepl',
  image: '/images/treneri/pavel2.jpg',
  content: [
    {
      type: 'ul',
      data: {
        align: 'start',
      },
      children: [
        {
          type: 'li',
          children: [
            { text: 'Absolvent kvalifikačního studia pro trenéry a porotce II. třídy' },
          ],
        },
        {
          type: 'li',
          children: [{ text: 'Tanečník mezinárodní třídy' }],
        },
        {
          type: 'li',
          children: [
            { text: 'Finalista Akademického mistrovství ČR ve standardních tancích' },
          ],
        },
        {
          type: 'li',
          children: [
            {
              text: 'Student doktorského studia na Fakultě tělesné kultury UP v Olomouci (zátěžová fyziologie; od roku 2021)',
            },
          ],
        },
        {
          type: 'li',
          children: [
            {
              text: 'Absolvent magisterského studia na Fakultě tělesné kultury UP v Olomouci (obor: Analýza pohybu)',
            },
          ],
        },
        {
          type: 'li',
          children: [{ text: 'Trenér atletiky III. třídy a plavání III. třídy' }],
        },
      ],
    },
  ] as MyValue,
};

const maruska = {
  name: 'Bc. Marie Hýžová ml.',
  image: '/images/treneri/maruska.jpg',
  content: [
    {
      type: 'ul',
      data: {
        align: 'start',
      },
      children: [
        {
          type: 'li',
          children: [
            { text: 'Absolventka kvalifikačního studia pro trenéry a porotce II. třídy' },
          ],
        },
        {
          type: 'li',
          children: [{ text: 'Finalistka mistrovství ČR v deseti tancích' }],
        },
        {
          type: 'li',
          children: [{ text: 'Semifinalistka mistrovství ČR ve standardních tancích' }],
        },
        {
          type: 'li',
          children: [
            { text: 'Čtvrtfinalistka mistrovství ČR v latinskoamerických tancích' },
          ],
        },
      ],
    },
  ] as MyValue,
};

const roman = {
  name: 'Bc. Roman Pecha',
  content: [
    {
      type: 'ul',
      data: {
        align: 'start',
      },
      children: [
        {
          type: 'li',
          children: [
            { text: 'Absolvent kvalifikačního studia pro trenéry a porotce II. třídy' },
          ],
        },
        {
          type: 'li',
          children: [{ text: 'Tanečník mezinárodní třídy' }],
        },
        {
          type: 'li',
          children: [
            { text: '3x 3. místo na MČR v kategorii Dospělí (v deseti tancích)' },
          ],
        },
        {
          type: 'li',
          children: [
            {
              text: '3x finalista mistrovství ČR v kategorii Dospělí (ve standardních tancích)',
            },
          ],
        },
        {
          type: 'li',
          children: [
            {
              text: '2x mistr ČR v kategorii U21 (v deseti tancích a ve standardních tancích)',
            },
          ],
        },
        {
          type: 'li',
          children: [
            {
              text: 'Čtvrtfinalista mistrovství světa a Evropy v kategorii mládež ve standardních tancích',
            },
          ],
        },
        {
          type: 'li',
          children: [
            {
              text: 'Čtvrtfinalista mistrovství světa v kategorii mládež a U21 v deseti tancích',
            },
          ],
        },
        {
          type: 'li',
          children: [{ text: 'Student MVŠO' }],
        },
      ],
    },
  ] as MyValue,
};

const hanka = {
  name: 'Hana Anna Šišková',
  content: [
    {
      type: 'ul',
      data: {
        align: 'start',
      },
      children: [
        {
          type: 'li',
          children: [
            { text: 'Absolventka kvalifikačního studia pro trenéry a porotce II. třídy' },
          ],
        },
        {
          type: 'li',
          children: [{ text: 'Tanečnice mezinárodní třídy' }],
        },
        {
          type: 'li',
          children: [
            { text: 'Finalistka mistrovství ČR v hlavní kategorii (10ti tanců)' },
          ],
        },
        {
          type: 'li',
          children: [
            {
              text: '4x finalistka mistrovství ČR v kategorii U21 (titul vicemistr ČR v deseti tancích, titul druhý vicemistr ČR ve standardních tancích a titul druhý vicemistr ČR v latinskoamerických tancích)',
            },
          ],
        },
        {
          type: 'li',
          children: [
            {
              text: 'Studentka Fakulty tělesné kultury na UP v Olomouci (obor: Tělesná výchova a sport)',
            },
          ],
        },
      ],
    },
  ] as MyValue,
};

const nela = {
  name: 'Nela Šírová',
  content: [
    {
      type: 'ul',
      data: {
        align: 'start',
      },
      children: [
        {
          type: 'li',
          children: [
            { text: 'Absolventka kvalifikačního studia pro trenéry a porotce II. třídy' },
          ],
        },
        {
          type: 'li',
          children: [{ text: 'Tanečnice mezinárodní třídy' }],
        },
        {
          type: 'li',
          children: [{ text: '3x finalistka mistrovství ČR juniorů a mládeže' }],
        },
        {
          type: 'li',
          children: [{ text: 'Studentka Lékařské fakulty na UP v Olomouci' }],
        },
      ],
    },
  ] as MyValue,
};

const matej = {
  name: 'Matěj Očenášek',
  image: '/images/treneri/matej.jpg',
  content: [
    {
      type: 'ul',
      data: {
        align: 'start',
      },
      children: [
        {
          type: 'li',
          children: [
            { text: 'Absolvent kvalifikačního studia pro trenéry a porotce III. třídy' },
          ],
        },
        {
          type: 'li',
          children: [{ text: '2x finalista mistrovství ČR mládeže' }],
        },
        {
          type: 'li',
          children: [
            {
              text: '3x finalista mistrovství ČR v kategorii U21 (vicemistr ČR v deseti tancích)',
            },
          ],
        },
        {
          type: 'li',
          children: [{ text: 'Student VUT FEKT' }],
        },
      ],
    },
  ] as MyValue,
};

const martin = {
  name: 'Martin Odstrčil',
  image: '/images/treneri/martin.jpg',
  content: [
    {
      type: 'ul',
      data: {
        align: 'start',
      },
      children: [
        {
          type: 'li',
          children: [{ text: 'Prezident DSP Kometa Brno' }],
        },
        {
          type: 'li',
          children: [
            {
              text: 'Trenér finalistů mistrovství světa a medailistů mistrovství Evropy',
            },
          ],
        },
        {
          type: 'li',
          children: [{ text: 'Trenér mistrů České republiky všech věkových kategorií' }],
        },
        {
          type: 'li',
          children: [{ text: '6x mistr ČR v deseti tancích (1995-2000)' }],
        },
        {
          type: 'li',
          children: [{ text: 'Mistr ČR ve standardních tancích (2000)' }],
        },
        {
          type: 'li',
          children: [{ text: 'Trenér a porotce I. třídy' }],
        },
        {
          type: 'li',
          children: [{ text: 'Porotce s nejvyšší mezinárodní licencí WDSF' }],
        },
        {
          type: 'li',
          children: [{ text: 'Porotce mistrovství světa a Evropy' }],
        },
      ],
    },
  ] as MyValue,
};

const pavla = {
  name: 'Pavla Landsfeldová',
  image: '/images/treneri/pavla.jpg',
  content: [
    {
      type: 'ul',
      data: {
        align: 'start',
      },
      children: [
        {
          type: 'li',
          children: [{ text: 'Trenérka mistrů České republiky ve standardních tancích' }],
        },
        {
          type: 'li',
          children: [{ text: 'Finalistka mistrovství ČSSR ve standardních tancích' }],
        },
        {
          type: 'li',
          children: [
            { text: 'Mistryně ČR ve standardních tancích v kategorii senior (1996)' },
          ],
        },
        {
          type: 'li',
          children: [{ text: 'Trenérka a porotkyně I. třídy' }],
        },
      ],
    },
  ] as MyValue,
};

const jerry = {
  name: 'Ing. Jaroslav Kučera',
  image: '/images/treneri/jerry.jpg',
  content: [
    {
      type: 'ul',
      data: {
        align: 'start',
      },
      children: [
        {
          type: 'li',
          children: [
            {
              text: 'Předseda trenérské rady projektu Sportovně talentované mládeže ČSTS',
            },
          ],
        },
        {
          type: 'li',
          children: [
            {
              text: 'Trenér finalistů mistrovství světa a medailistů mistrovství Evropy',
            },
          ],
        },
        {
          type: 'li',
          children: [{ text: 'Trenér mistrů České republiky všech věkových kategorií' }],
        },
        {
          type: 'li',
          children: [
            { text: 'Vicemistr ČR v latinskoamerických tancích (1992, 1993, 1995)' },
          ],
        },
        {
          type: 'li',
          children: [{ text: 'Finalista mistrovství ČR v deseti tancích (1993)' }],
        },
        {
          type: 'li',
          children: [{ text: 'Trenér a porotce i. třídy' }],
        },
        {
          type: 'li',
          children: [{ text: 'Porotce s nejvyšší mezinárodní licencí WDSF' }],
        },
        {
          type: 'li',
          children: [{ text: 'Porotce mistrovství Evropy' }],
        },
      ],
    },
  ] as MyValue,
};

const internal: {
  name: string;
  image?: string;
  content: MyValue;
}[] = [mirek, filip, marie, lucka, grepi, maruska, roman, hanka, nela, matej];

const external: {
  name: string;
  image?: string;
  content: MyValue;
}[] = [martin, pavla, jerry];
