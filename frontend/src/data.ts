import { getPlaceholder } from './test-utils';

export const useLocations = () => [
  {
    image: getPlaceholder(360, 240),
    name: 'Taneční centrum při FZŠ Holečkova',
    address: 'Holečkova 10, 779 00, Olomouc (vchod brankou u zastávy Povel - škola)',
    href: 'https://www.zsholeckova.cz/',
    mapHref: 'https://goo.gl/maps/swv3trZB2uvjcQfR6',
    map: {
      lat: 49.57963,
      lng: 17.2495939,
      zoom: 12,
    },
  },
  {
    image: getPlaceholder(360, 240),
    name: 'Tělocvična Slovanského gymnázia',
    address: 'Jiřího z Poděbrad 13, 779 00 Olomouc (vchod brankou z ulice U reálky)',
    href: 'https://www.sgo.cz/',
    mapHref: 'https://goo.gl/maps/PgsEra8TnYV4V7KGA',
    map: {
      lat: 49.59490,
      lng: 17.26340,
      zoom: 12,
    },
  },
  {
    image: getPlaceholder(360, 240),
    name: 'T.J. Sokol Přerov',
    address: 'Brabansko 2, 750 02 Přerov',
    href: 'https://www.sokolprerov.cz/',
    mapHref: 'https://goo.gl/maps/gP5cDBxJwgUS3hyz6',
    map: {
      lat: 49.4574331,
      lng: 17.4480036,
      zoom: 12,
    },
  },
  {
    image: getPlaceholder(360, 240),
    name: 'Taneční sál Gala',
    address: 'Západní 1, 796 04 Prostějov-Krasice (vchod vedle podnikové prodejny Gala)',
    href: null,
    mapHref: 'https://goo.gl/maps/Jtv6mdoSgBEdsiTN7',
    map: {
      lat: 49.4681836,
      lng: 17.0837344,
      zoom: 12,
    },
  },
];

export const useServices = () => [
  {
    image: '/images/services-pripravka.png',
    href: null,
    header: "Přípravka tanečního sportu",
    text: "První kroky do světa tanečního sportu pro děti od 5 do 10 let. Všeobecná taneční průprava a základy tanečních kroků pro budoucí hvězdy."
  },
  {
    image: '/images/services-pro-deti.png',
    href: null,
    header: "Základy tanečního sportu",
    text: "Tréninkové programy pro začínající a mírně pokročilé tanečníky ve věkových skupinách juniorů (12-15 let), mládež a dospělí (16+ let)."
  },
  {
    image: '/images/services-pro-zacatecniky.png',
    href: null,
    header: "Výkonnostní sport",
    text: "Tréninkové programy pro soutěžní tanečníky ve všech věkových skupinách a výkonnostních třídách uzpůsobené podle potřeb v jednotlivých výkonnostních stupních."
  },
  {
    image: '/images/services-soutezni.png',
    href: null,
    header: "Sportovní centrum mládeže",
    text: "Tréninkový program pro vrcholové sportovce, reprezentanty ČR se špičkovými českými trenéry, speciální kondiční přípravou a moderními metodami sportovního tréninku. Jsme jediným klubem v Olomouckém kraji se statutem Sprtovního centra mládeže dle MŠMT."
  },
];

export const useInternalTrainers = () => [
  {
    img: '/images/treneri/mirek2.jpg',
    name: 'Mgr. Miroslav Hýža',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Předseda, šéftrenér TK Olymp' },
        { text: 'Trenér mistrů ČR' },
        { text: 'Vicemistr ČR ve standardních tancích v hlavní kategorii 2018' },
        { text: '3x mistr ČR v kategorii juniorů' },
        { text: 'Finalista Akademického mistrovství Evropy 2016' },
        { text: 'Držitel ocenění TOP 30 sportovních trenérů mládeže 2017' },
      ]
    }],
  },
  {
    img: '/images/treneri/filip.jpg',
    name: 'Ing. Filip Karásek',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Trenér a porotce II. třídy' },
        { text: '2x mistr ČR v latinsko-amerických tancích v kategorii profesionálů' },
        { text: '3x mistr ČR v latinsko-amerických tancích v hlavní kategorii' },
        { text: 'Semifinalista mistrovství světa a Evropy v kategorii profesionálů' },
        { text: 'Semifinalista mistrovství Evropy v hlavní kategorii' },
        { text: 'Čtvrtfinalista mistrovství světa v hlavní kategorii' },
        { text: 'Finalista GOC ve Stuttgartu kategorii profesionálů' },
        { text: 'Semifinalista Světových her v Kolumbii' },
        { text: 'Předseda Komise sportovců (or r. 2017)' },
      ]
    }],
  },
  {
    img: '/images/treneri/marie2.jpg',
    name: 'Mgr. Marie Hýžová',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Trenér a porotce I. třídy' },
        { text: 'Dlouholetá tanečnice mezinárodní třídy' },
        { text: 'Zkušená trenérka dětských a juniorských přípravek' },
        { text: 'Švadlena tanečního oblečení (řadu let šatí finalisty i medailisty mistrovství ČR)' },
      ]
    }],
  },
  {
    img: '/images/treneri/lucka3.jpg',
    name: 'Mgr. Lucie Benýšková',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Absolventka kvalifikačního studia pro trenéry a porotce II.třídy' },
        { text: 'Semifinalistka mistrovství ČR v hlavní kategorii (v deseti tancích a ve standardních tancích)' },
        { text: '3x finalistka mistrovství ČR juniorů a mládeže (titul druhý vicemistr ČR ve standardních tancích v kategorii mládež)' },
        { text: '2. místo v Taneční lize (žebříček párů mezinárodní třídy)' },
        { text: 'Trenérka dětských a juniorských přípravek (12 let praxe)' },
        { text: 'Trenérka finalistů mistrovství ČR juniorů' },
        { text: 'Absolventka Fakulty tělesné kultury na UP v Olomouci (obor: Tělesná výchova a sport)' },
      ]
    }],
  },
  {
    img: '/images/treneri/pavel2.jpg',
    name: 'Mgr. Pavel Grepl',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Absolvent kvalifikačního studia pro trenéry a porotce II. třídy' },
        { text: 'Tanečník mezinárodní třídy' },
        { text: 'Finalista Akademického mistrovství ČR ve standardních tancích' },
        { text: 'Student Fakulty tělesné kultury na UP v Olomouci (obor: Trenérství a sport)' },
        { text: 'Trenér atletiky III. třídy a plavání III. třídy' },
      ]
    }],
  },
  {
    img: '/images/treneri/maruska.jpg',
    name: 'Bc. Marie Hýžová ml.',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Absolventka kvalifikačního studia pro trenéry a porotce II. třídy' },
        { text: 'Finalistka mistrovství ČR v deseti tancích' },
        { text: 'Semifinalistka mistrovství ČR ve standardních tancích' },
        { text: 'Čtvrtfinalistka mistrovství ČR v latinskoamerických tancích' },
      ]
    }],
  },
  {
    img: getPlaceholder(360, 240),
    name: 'Roman Pecha',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Absolvent kvalifikačního studia pro trenéry a porotce II. třídy' },
        { text: 'Tanečník mezinárodní třídy' },
        { text: 'Finalista mistrovství ČR v hlavní kategorii (ve standardních tancích)' },
        { text: '2x mistr ČR v kategorii U21 (v deseti tancích a ve standardních tancích)' },
        { text: 'Čtvrtfinalista mistrovství světa a Evropy v kategorii mládež ve standardních tancích' },
        { text: 'Čtvrtfinalista mistrovství světa v kategorii mládež a U21 v deseti tancích' },
        { text: 'Student MVŠO' },
      ]
    }],
  },
  {
    img: getPlaceholder(360, 240),
    name: 'Hana Anna Šišková',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Absolventka kvalifikačního studia pro trenéry a porotce III. třídy' },
        { text: 'Tanečnice mezinárodní třídy' },
        { text: '4x finalistka mistrovství ČR v kategorii U21 (titul vicemistr ČR v deseti tancích, titul druhý vicemistr ČR ve standardních tancích a titul druhý vicemistr ČR v latinskoamerických tancích)' },
        { text: 'Studentka Fakulty tělesné kultury na UP v Olomouci (obor: Tělesná výchova a sport)' },
      ]
    }],
  },
  {
    img: getPlaceholder(360, 240),
    name: 'Nela Šírová',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Absolventka kvalifikačního studia pro trenéry a porotce III. třídy' },
        { text: 'Tanečnice mezinárodní třídy' },
        { text: '3x finalistka mistrovství ČR juniorů a mládeže' },
        { text: 'Studentka Lékařské fakulty na UP v Olomouci' },
      ]
    }],
  },
  {
    img: '/images/treneri/matej.jpg',
    name: 'Matěj Očenášek',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Absolvent kvalifikačního studia pro trenéry a porotce III. třídy' },
        { text: '2x finalista mistrovství ČR mládeže' },
        { text: '3x finalista mistrovství ČR v kategorii U21 (vicemistr ČR v deseti tancích)' },
        { text: 'Student VUT FEKT' },
      ]
    }],
  },
];

export const useExternalTrainers = () => [
  {
    img: '/images/treneri/martin.jpg',
    name: 'Martin Odstrčil',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Prezident DSP Kometa Brno' },
        { text: 'Trenér finalistů mistrovství světa a medailistů mistrovství Evropy' },
        { text: 'Trenér mistrů České republiky všech věkových kategorií' },
        { text: '6x mistr ČR v deseti tancích (1995-2000)' },
        { text: 'Mistr ČR ve standardních tancích (2000)' },
        { text: 'Trenér a porotce I. třídy' },
        { text: 'Porotce s nejvyšší mezinárodní licencí WDSF' },
        { text: 'Porotce mistrovství světa a Evropy' },
      ]
    }],
  },
  {
    img: '/images/treneri/pavla.jpg',
    name: 'Pavla Landsfeldová',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Trenérka mistrů České republiky ve standardních tancích' },
        { text: 'Finalistka mistrovství ČSSR ve standardních tancích' },
        { text: 'Mistryně ČR ve standardních tancích v kategorii senior (1996)' },
        { text: 'Trenérka a porotkyně I. třídy' },
      ]
    }],
  },
  {
    img: '/images/treneri/jerry.jpg',
    name: 'Ing. Jaroslav Kučera',
    content: [{
      type: 'bulleted-list',
      children: [
        { text: 'Předseda trenérské rady projektu Sportovně talentované mládeže ČSTS' },
        { text: 'Trenér finalistů mistrovství světa a medailistů mistrovství Evropy' },
        { text: 'Trenér mistrů České republiky všech věkových kategorií' },
        { text: 'Vicemistr ČR v latinskoamerických tancích (1992, 1993, 1995)' },
        { text: 'Finalista mistrovství ČR v deseti tancích (1993)' },
        { text: 'Trenér a porotce i. třídy' },
        { text: 'Porotce s nejvyšší mezinárodní licencí WDSF' },
        { text: 'Porotce mistrovství Evropy' },
      ]
    }],
  },
];

export const useHeroData = () => [
  {
    img: getPlaceholder(1600, 900),
    href: "/o-nas",
    text: "Mistři České republiky v tanečním sportu 2019",
  },
  {
    img: getPlaceholder(1600, 900),
    href: "/o-nas",
    text: "Mistři České republiky v tanečním sportu 2020",
  },
  {
    img: getPlaceholder(1600, 900),
    href: "/o-nas",
    text: "Mistři České republiky v tanečním sportu 2021",
  },
];
