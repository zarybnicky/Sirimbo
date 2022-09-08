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

export const useExternalTrainers = () => [
  {
    image: '',
    name: '',
    content: [{
      type: 'bulleted-list',
      children: [
      ]
    }],
  },
  {
    image: '',
    name: '',
    content: [{
      type: 'bulleted-list',
      children: [
      ]
    }],
  },
  {
    image: '',
    name: '',
    content: [{
      type: 'bulleted-list',
      children: [
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
