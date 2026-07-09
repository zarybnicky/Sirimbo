import { TenantConfig } from '../types';

export const config: TenantConfig = {
  shortName: 'TK Olymp',
  copyrightLine: '© 2024 TK Olymp Olomouc, z. s.',
  favicon: '',
  seo: {
    titleTemplate: '%s · TK Olymp',
    defaultTitle: 'TK Olymp',
    description:
      'TK Olymp Olomouc vede děti, mládež i dospělé v tanečním sportu v Olomouci a Prostějově. Nabízíme tréninkové programy, soutěžní tanec i školní kroužky.',
    themeColor: '#000',
    facebook: { appId: '704526480597551' },
    openGraph: {
      siteName: 'TK Olymp',
      locale: 'cs_CZ',
      type: 'website',
    },
    additionalMetaTags: [
      { name: 'wot-verification', content: 'ec0cf41ab42dae52d3d4' },
      { name: 'msvalidate.01', content: '7BD6C8B5748FC22EF06AB3AE89900885' },
      {
        name: 'facebook-domain-verification',
        content: 'k8tt64a93roxiymxo79clpvklan9j2',
      },
      {
        name: 'google-site-verification',
        content: 'Hfe7zlgTDOIpJv4rKGQz2Xg8Aezb6sIO0aAxVhrml9w',
      },
      {
        name: 'norton-safeweb-site-verification',
        content:
          'r44xj2vskhlgkyqcqm1hdgga2jdfj-idvyys0277y96s72k-tq0z-yyjdu7h3el6pi2gek0i4ykq3xgiguufrvuhj8nbj4n4miwjhvumhp35jfrafyynhj4ee8ctzpzh',
      },
      { name: 'viewport', content: 'initial-scale=1,width=device-width' },
    ],
    additionalLinkTags: [
      {
        rel: 'apple-touch-icon',
        sizes: '180x180',
        href: '/olymp/apple-touch-icon.png?v=3',
      },
      { rel: 'icon', sizes: '32x32', href: '/olymp/favicon-32x32.png?v=3' },
      { rel: 'icon', sizes: '16x16', href: '/olymp/favicon-16x16.png?v=3' },
      { rel: 'shortcut icon', href: '/olymp/favicon.ico?v=3' },
      { rel: 'manifest', href: '/olymp/site.webmanifest?v=3' },
      {
        rel: 'mask-icon',
        color: '#5bbad5',
        href: '/olymp/safari-pinned-tab.svg?v=3',
      },
    ],
  },
  publicSite: {
    origin: 'https://tkolymp.cz',
    locale: 'cs-CZ',
    image: {
      url: '/images/2023-04-MCRDruzstev.jpg',
      width: 6373,
      height: 3314,
      alt: 'Taneční klub Olymp Olomouc na mistrovství České republiky družstev',
    },
    organization: {
      name: 'TK Olymp Olomouc',
      legalName: 'Taneční klub Olymp Olomouc, z. s.',
      logo: '/olymp/android-chrome-512x512.png',
      email: 'miroslav.hyza@tkolymp.cz',
      telephone: '+420737545525',
      sameAs: [
        'https://www.facebook.com/tkolymp',
        'https://www.instagram.com/tanecni_klub_olymp',
        'https://www.youtube.com/user/TheMamcro',
      ],
      address: {
        streetAddress: 'Jiráskova 381/25',
        addressLocality: 'Olomouc - Hodolany',
        postalCode: '779 00',
        addressCountry: 'CZ',
      },
    },
  },
  enableRegistration: true,
  enableStarletImport: false,
  useTrainerInitials: false,
  lockEventsByDefault: false,
  facebookPixelId: '704526480597551',
};
