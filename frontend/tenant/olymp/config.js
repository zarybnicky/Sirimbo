 /**
 * @type {import("../types").Config}
 */
module.exports = {
  shortName: "TK Olymp",
  copyrightLine: "© 2024 TK Olymp Olomouc, z. s.",
  favicon: '',
  enableHome: true,
  enableArticles: true,
  enableRecruitmentLink: true,
  enableStarletImport: false,
  useTrainerInitials: false,
  lockEventsByDefault: false,
  themePrimary: '#ed1734',
  themeAccent: 'red',
  themeNeutral: 'gray',
  facebookPixelId: '704526480597551',
  publicNavigation: [
    { title: 'Domů', href: '/' },
    {
      title: 'Klub',
      children: [
        { title: 'O nás', href: '/o-nas' },
        { title: 'Kde trénujeme', href: '/kde-trenujeme' },
        { title: 'Tréninkové programy', href: '/treninkove-programy' },
        { title: 'Trenéři', href: '/treneri' },
        { title: 'Výhody členství', href: '/vyhody-clenstvi' },
        { title: 'Galerie mistrů', href: '/galerie-mistru' },
      ],
    },
    {
      title: 'Nabízíme',
      children: [
        {
          title: 'Přípravka tanečního sportu',
          href: 'https://nabor.tkolymp.cz',
          recruitmentLink: true,
        },
        { title: 'Vystoupení na akcích', href: '/vystoupeni' },
        { title: 'Školní taneční kroužky', href: '/skolni-krouzky' },
      ],
    },
    { title: 'Aktuality', href: '/clanky' },
    { title: 'Galerie', href: '/galerie' },
    { title: 'Akce', href: '/akce' },
    { title: 'Kontakt', href: '/kontakt' },
  ],
};
