import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
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
            {item.content}
          </TrainerCard>
        ))}
      </div>

      <div className="col-feature mt-16 prose">
        <h2>Externí trenéři</h2>
      </div>

      <div className="col-feature mt-4 mb-16 grid gap-8 lg:grid-cols-2">
        {external.map((item) => (
          <TrainerCard key={item.name} name={item.name} image={item.image}>
            {item.content}
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
  content: (
    <div className="prose">
      <ul>
        <li>Předseda, šéftrenér TK Olymp</li>
        <li>Trenér mistrů ČR</li>
        <li>Vicemistr ČR ve standardních tancích v hlavní kategorii 2018</li>
        <li>3x mistr ČR v kategorii juniorů</li>
        <li>Finalista Akademického mistrovství Evropy 2016</li>
        <li>Držitel ocenění TOP 30 sportovních trenérů mládeže 2017</li>
      </ul>
    </div>
  ),
};

const filip = {
  name: 'Ing. Filip Karásek',
  image: '/images/treneri/filip.jpg',
  content: (
    <div className="prose">
      <ul>
        <li>Trenér a porotce I. třídy</li>
        <li>3x mistr ČR v latinsko-amerických tancích v kategorii Dopspělí</li>
        <li>2x mistr ČR v latinsko-amerických tancích v kategorii Profesionálů</li>
        <li>Čtvrtfinalista mistrovství světa a Evropy v kategorii Dospělí</li>
        <li>Semifinalista mistrovství světa a Evropy v kategorii Profesionálové</li>
        <li>Finalista German Open Championship (Stuttgart) v kategorii Profesionálové</li>
        <li>Semifinalista Světových her v Kolumbii v roce 2013</li>
        <li>Předseda Komise sportovců ČSTS (2017-2019)</li>
        <li>Viceprezident ČSTS (od roku 2019)</li>
      </ul>
    </div>
  ),
};

const marie = {
  name: 'Mgr. Marie Hýžová',
  image: '/images/treneri/marie2.jpg',
  content: (
    <div className="prose">
      <ul>
        <li>Trenér a porotce I. třídy</li>
        <li>Dlouholetá tanečnice mezinárodní třídy</li>
        <li>Zkušená trenérka dětských a juniorských přípravek</li>
        <li>
          Švadlena tanečního oblečení (řadu let šatí finalisty i medailisty mistrovství
          ČR)
        </li>
      </ul>
    </div>
  ),
};

const lucka = {
  name: 'Mgr. Lucie Benýšková',
  image: '/images/treneri/lucka3.jpg',
  content: (
    <div className="prose">
      <ul>
        <li>Absolventka kvalifikačního studia pro trenéry a porotce II.třídy</li>
        <li>
          Semifinalistka mistrovství ČR v hlavní kategorii (v deseti tancích a ve
          standardních tancích)
        </li>
        <li>
          3x finalistka mistrovství ČR juniorů a mládeže (titul druhý vicemistr ČR ve
          standardních tancích v kategorii mládež)
        </li>
        <li>2. místo v Taneční lize (žebříček párů mezinárodní třídy)</li>
        <li>Trenérka dětských a juniorských přípravek (12 let praxe)</li>
        <li>Trenérka finalistů mistrovství ČR juniorů</li>
        <li>
          Absolventka Fakulty tělesné kultury na UP v Olomouci (obor: Tělesná výchova a
          sport)
        </li>
      </ul>
    </div>
  ),
};

const grepi = {
  name: 'Mgr. Pavel Grepl',
  image: '/images/treneri/pavel2.jpg',
  content: (
    <div className="prose">
      <ul>
        <li>Absolvent kvalifikačního studia pro trenéry a porotce II. třídy</li>
        <li>Tanečník mezinárodní třídy</li>
        <li>Finalista Akademického mistrovství ČR ve standardních tancích</li>
        <li>
          Student doktorského studia na Fakultě tělesné kultury UP v Olomouci (zátěžová
          fyziologie; od roku 2021)
        </li>
        <li>
          Absolvent magisterského studia na Fakultě tělesné kultury UP v Olomouci (obor:
          Analýza pohybu)
        </li>
        <li>Trenér atletiky III. třídy a plavání III. třídy</li>
      </ul>
    </div>
  ),
};

const maruska = {
  name: 'Bc. Marie Hýžová ml.',
  image: '/images/treneri/maruska.jpg',
  content: (
    <div className="prose">
      <ul>
        <li>Absolventka kvalifikačního studia pro trenéry a porotce II. třídy</li>
        <li>Finalistka mistrovství ČR v deseti tancích</li>
        <li>Semifinalistka mistrovství ČR ve standardních tancích</li>
        <li>Čtvrtfinalistka mistrovství ČR v latinskoamerických tancích</li>
      </ul>
    </div>
  ),
};

const roman = {
  name: 'Bc. Roman Pecha',
  content: (
    <div className="prose">
      <ul>
        <li>Absolvent kvalifikačního studia pro trenéry a porotce II. třídy</li>
        <li>Tanečník mezinárodní třídy</li>
        <li>3x 3. místo na MČR v kategorii Dospělí (v deseti tancích)</li>
        <li>3x finalista mistrovství ČR v kategorii Dospělí (ve standardních tancích)</li>
        <li>2x mistr ČR v kategorii U21 (v deseti tancích a ve standardních tancích)</li>
        <li>
          Čtvrtfinalista mistrovství světa a Evropy v kategorii mládež ve standardních
          tancích
        </li>
        <li>
          Čtvrtfinalista mistrovství světa v kategorii mládež a U21 v deseti tancích
        </li>
        <li>Student MVŠO</li>
      </ul>
    </div>
  ),
};

const hanka = {
  name: 'Hana Anna Šišková',
  content: (
    <div className="prose">
      <ul>
        <li>Absolventka kvalifikačního studia pro trenéry a porotce II. třídy</li>
        <li>Tanečnice mezinárodní třídy</li>
        <li>Finalistka mistrovství ČR v hlavní kategorii (10ti tanců)</li>
        <li>
          4x finalistka mistrovství ČR v kategorii U21 (titul vicemistr ČR v deseti
          tancích, titul druhý vicemistr ČR ve standardních tancích a titul druhý
          vicemistr ČR v latinskoamerických tancích)
        </li>
        <li>
          Studentka Fakulty tělesné kultury na UP v Olomouci (obor: Tělesná výchova a
          sport)
        </li>
      </ul>
    </div>
  ),
};

const nela = {
  name: 'Nela Šírová',
  content: (
    <div className="prose">
      <ul>
        <li>Absolventka kvalifikačního studia pro trenéry a porotce II. třídy</li>
        <li>Tanečnice mezinárodní třídy</li>
        <li>3x finalistka mistrovství ČR juniorů a mládeže</li>
        <li>Studentka Lékařské fakulty na UP v Olomouci</li>
      </ul>
    </div>
  ),
};

const matej = {
  name: 'Matěj Očenášek',
  image: '/images/treneri/matej.jpg',
  content: (
    <div className="prose">
      <ul>
        <li>Absolvent kvalifikačního studia pro trenéry a porotce III. třídy</li>
        <li>2x finalista mistrovství ČR mládeže</li>
        <li>
          3x finalista mistrovství ČR v kategorii U21 (vicemistr ČR v deseti tancích)
        </li>
        <li>Student VUT FEKT</li>
      </ul>
    </div>
  ),
};

const martin = {
  name: 'Martin Odstrčil',
  image: '/images/treneri/martin.jpg',
  content: (
    <div className="prose">
      <ul>
        <li>Prezident DSP Kometa Brno</li>
        <li>Trenér finalistů mistrovství světa a medailistů mistrovství Evropy</li>
        <li>Trenér mistrů České republiky všech věkových kategorií</li>
        <li>6x mistr ČR v deseti tancích (1995-2000)</li>
        <li>Mistr ČR ve standardních tancích (2000)</li>
        <li>Trenér a porotce I. třídy</li>
        <li>Porotce s nejvyšší mezinárodní licencí WDSF</li>
        <li>Porotce mistrovství světa a Evropy</li>
      </ul>
    </div>
  ),
};

const pavla = {
  name: 'Pavla Landsfeldová',
  image: '/images/treneri/pavla.jpg',
  content: (
    <div className="prose">
      <ul>
        <li>Trenérka mistrů České republiky ve standardních tancích</li>
        <li>Finalistka mistrovství ČSSR ve standardních tancích</li>
        <li>Mistryně ČR ve standardních tancích v kategorii senior (1996)</li>
        <li>Trenérka a porotkyně I. třídy</li>
      </ul>
    </div>
  ),
};

const jerry = {
  name: 'Ing. Jaroslav Kučera',
  image: '/images/treneri/jerry.jpg',
  content: (
    <div className="prose">
      <ul>
        <li>Předseda trenérské rady projektu Sportovně talentované mládeže ČSTS</li>
        <li>Trenér finalistů mistrovství světa a medailistů mistrovství Evropy</li>
        <li>Trenér mistrů České republiky všech věkových kategorií</li>
        <li>Vicemistr ČR v latinskoamerických tancích (1992, 1993, 1995)</li>
        <li>Finalista mistrovství ČR v deseti tancích (1993)</li>
        <li>Trenér a porotce i. třídy</li>
        <li>Porotce s nejvyšší mezinárodní licencí WDSF</li>
        <li>Porotce mistrovství Evropy</li>
      </ul>
    </div>
  ),
};

const internal: {
  name: string;
  image?: string;
  content: React.ReactNode;
}[] = [mirek, filip, marie, lucka, grepi, maruska, roman, hanka, nela, matej];

const external: {
  name: string;
  image?: string;
  content: React.ReactNode;
}[] = [martin, pavla, jerry];
