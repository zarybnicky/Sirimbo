import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { Layout } from 'components/layout/Layout';
import Image, { StaticImageData } from 'next/image';

type TrainerCardProps = {
  image?: StaticImageData;
  name: string;
  children: React.ReactNode;
};

export function TrainerCard(props: TrainerCardProps) {
  return (
    <div className="mb-14 relative">
      {props.image && (
      <div className="shrink-0 relative ml-8 lg:ml-0 mb-4 lg:mb-0 lg:absolute lg:-left-44 lg:top-10 w-[200px] h-[266px] lg:w-[150px] lg:h-[200px]">
        <Image
          className="object-cover drop-shadow"
          src={props.image}
          alt={props.name}
          fill
        />
      </div>
      )}
      <h3 className="text-4xl mb-4 text-red-500 drop-shadow">{props.name}</h3>
      <div className="prose">{props.children}</div>
    </div>
  );
}

export default function AboutPage() {
  return (
    <>
      <div className="my-12">
        <h1 className="mb-8 text-5xl text-red-500 drop-shadow tracking-wide">
          Naši trenéři
        </h1>

        <p className="prose">
          Taneční klub se pyšní týmem trenérů, kteří představují základ dlouholeté úspěšné
          historie. Trenéři disponují vysokou odborností, bohatými zkušenostmi a vášní pro
          taneční sport. Stále se snažíme zdokonalovat a držet krok s nejnovějšími trendy
          a technikami v tanečním světě. Pravidelně se účastníme školení a workshopů,
          které nám umožňují rozšiřovat naše znalosti a dovednosti. S našimi trenéry
          můžete mít jistotu, že jste v péči profesionálů, kteří jsou oddaní vašemu
          osobnímu rozvoji a úspěchu v tanečním sportu.
        </p>
      </div>

      {internal.map((item) => (
        <TrainerCard key={item.name} name={item.name} image={item.image}>
          {item.children}
        </TrainerCard>
      ))}

      <div className="my-12">
        <h1 className="mb-8 text-5xl text-red-500 drop-shadow tracking-wide">
          Externí trenéři
        </h1>

        <p className="prose">
          V rámci klubu spolupracujeme s významnými odborníky tanečního světa z různých
          částí republiky, kteří s námi sdílí své znalosti a dovednosti. Externí odborníci
          přinášejí nové perspektivy, inovativní přístupy a inspiraci, které nám pomáhají
          posunout naše tréninkové metody a standardy ještě dále. Spolupráce s těmito
          významnými osobnostmi je pro náš klub klíčová a umožňuje nám nabídnout našim
          členům zkušenosti a know-how z nejlepších zdrojů v našem oboru.
        </p>
      </div>

      {external.map((item) => (
        <TrainerCard key={item.name} name={item.name} image={item.image}>
          {item.children}
        </TrainerCard>
      ))}

      <CallToAction />
    </>
  );
}

AboutPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;

const mirek: TrainerCardProps = {
  name: 'Mgr. Miroslav Hýža',
  image: imgMirek,
  children: (
    <ul>
      <li>Předseda, šéftrenér TK Olymp</li>
      <li>Trenér mistrů ČR</li>
      <li>Vicemistr ČR ve standardních tancích v hlavní kategorii 2018</li>
      <li>3x mistr ČR v kategorii juniorů</li>
      <li>Finalista Akademického mistrovství Evropy 2016</li>
      <li>Držitel ocenění TOP 30 sportovních trenérů mládeže 2017</li>
    </ul>
  ),
};

const filip: TrainerCardProps = {
  name: 'Ing. Filip Karásek',
  image: imgFilip,
  children: (
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
  ),
};

const marie: TrainerCardProps = {
  name: 'Mgr. Marie Hýžová',
  image: imgMarie,
  children: (
    <ul>
      <li>Trenér a porotce I. třídy</li>
      <li>Dlouholetá tanečnice mezinárodní třídy</li>
      <li>Zkušená trenérka dětských a juniorských přípravek</li>
      <li>
        Švadlena tanečního oblečení (řadu let šatí finalisty i medailisty mistrovství ČR)
      </li>
    </ul>
  ),
};

const lucka: TrainerCardProps = {
  name: 'Mgr. Lucie Benýšková',
  image: imgLucka,
  children: (
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
  ),
};

const grepi: TrainerCardProps = {
  name: 'Mgr. Pavel Grepl',
  image: imgPavel,
  children: (
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
  ),
};

const maruska: TrainerCardProps = {
  name: 'Bc. Marie Hýžová ml.',
  image: imgMaruska,
  children: (
    <ul>
      <li>Absolventka kvalifikačního studia pro trenéry a porotce II. třídy</li>
      <li>Finalistka mistrovství ČR v deseti tancích</li>
      <li>Semifinalistka mistrovství ČR ve standardních tancích</li>
      <li>Čtvrtfinalistka mistrovství ČR v latinskoamerických tancích</li>
    </ul>
  ),
};

const roman: TrainerCardProps = {
  name: 'Bc. Roman Pecha',
  image: imgRoman,
  children: (
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
      <li>Čtvrtfinalista mistrovství světa v kategorii mládež a U21 v deseti tancích</li>
      <li>Student MVŠO</li>
    </ul>
  ),
};

const hanka: TrainerCardProps = {
  name: 'Hana Anna Šišková',
  image: imgHanka,
  children: (
    <ul>
      <li>Absolventka kvalifikačního studia pro trenéry a porotce II. třídy</li>
      <li>Tanečnice mezinárodní třídy</li>
      <li>Finalistka mistrovství ČR v hlavní kategorii (10ti tanců)</li>
      <li>
        4x finalistka mistrovství ČR v kategorii U21 (titul vicemistr ČR v deseti tancích,
        titul druhý vicemistr ČR ve standardních tancích a titul druhý vicemistr ČR v
        latinskoamerických tancích)
      </li>
      <li>
        Studentka Fakulty tělesné kultury na UP v Olomouci (obor: Tělesná výchova a sport)
      </li>
    </ul>
  ),
};

const nela: TrainerCardProps = {
  name: 'Nela Šírová',
  image: imgNela,
  children: (
    <ul>
      <li>Absolventka kvalifikačního studia pro trenéry a porotce II. třídy</li>
      <li>Tanečnice mezinárodní třídy</li>
      <li>3x finalistka mistrovství ČR juniorů a mládeže</li>
      <li>Studentka Lékařské fakulty na UP v Olomouci</li>
    </ul>
  ),
};

import imgMirek from 'public/images/treneri-2023/mirek.jpg';
import imgFilip from 'public/images/treneri-2023/filip.jpg';
import imgMarie from 'public/images/treneri-2023/marie.jpg';
import imgHanka from 'public/images/treneri-2023/hanka.jpg';
import imgRoman from 'public/images/treneri-2023/roman.jpg';
import imgNela from 'public/images/treneri-2023/nela.jpg';
import imgLucka from 'public/images/treneri/lucka3.jpg';
import imgPavel from 'public/images/treneri/pavel2.jpg';
import imgMaruska from 'public/images/treneri/maruska.jpg';
import imgMatej from 'public/images/treneri/matej.jpg';
import imgMartin from 'public/images/treneri-2023/martin.jpg';
import imgDavid from 'public/images/treneri-2023/david.jpg';
import imgTara from 'public/images/treneri-2023/tara.jpg';
import imgPavla from 'public/images/treneri/pavla.jpg';
import imgJerry from 'public/images/treneri/jerry.jpg';

const matej: TrainerCardProps = {
  name: 'Matěj Očenášek',
  image: imgMatej,
  children: (
    <ul>
      <li>Absolvent kvalifikačního studia pro trenéry a porotce III. třídy</li>
      <li>2x finalista mistrovství ČR mládeže</li>
      <li>3x finalista mistrovství ČR v kategorii U21 (vicemistr ČR v deseti tancích)</li>
      <li>Student VUT FEKT</li>
    </ul>
  ),
};

const martin: TrainerCardProps = {
  name: 'Martin Odstrčil',
  image: imgMartin,
  children: (
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
  ),
};


const david: TrainerCardProps = {
  name: 'David Odstrčil',
  image: imgDavid,
  children: (
    <ul>
      <li>Mistr světa v deseti tancích (2022)</li>
      <li>2. vicemistr světa ve standardních tancích (2023)</li>
      <li>6x mistr ČR ve standardních tancích (2015-2021)</li>
      <li>7x mistr ČR ve standardních tancích (2013-2019)</li>
      <li>Trenér a porotce II. třídy</li>
    </ul>
  ),
};


const tara: TrainerCardProps = {
  name: 'Tara Bohak',
  image: imgTara,
  children: (
    <ul>
      <li>Mistryně světa v deseti tancích (2022)</li>
      <li>2. vicemistryně světa ve standardních tancích (2023)</li>
      <li>6x mistryně ČR ve standardních tancích (2015-2021)</li>
      <li>7x mistryně ČR ve standardních tancích (2013-2019)</li>
      <li>Trenérka a porotce III. třídy</li>
    </ul>
  ),
};

const pavla: TrainerCardProps = {
  name: 'Pavla Landsfeldová',
  image: imgPavla,
  children: (
    <ul>
      <li>Trenérka mistrů České republiky ve standardních tancích</li>
      <li>Finalistka mistrovství ČSSR ve standardních tancích</li>
      <li>Mistryně ČR ve standardních tancích v kategorii senior (1996)</li>
      <li>Trenérka a porotkyně I. třídy</li>
    </ul>
  ),
};

const jerry: TrainerCardProps = {
  name: 'Ing. Jaroslav Kučera',
  image: imgJerry,
  children: (
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
  ),
};

const internal = [mirek, filip, marie, roman, hanka, nela, lucka, grepi, maruska, matej];
const external = [martin, david, tara, pavla, jerry];
