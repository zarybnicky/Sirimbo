 /**
 * @type {import("../types").Config}
 */
module.exports = {
  shortName: "DSP Kometa",
  copyrightLine: "© 2024 DSP Kometa Brno, z. s.",
  favicon: '',
  enableHome: false,
  enableArticles: false,
  enableRecruitmentLink: false,
  enableStarletImport: false,
  useTrainerInitials: false,
  lockEventsByDefault: false,
  themePrimary: '#be9f69',
  themeAccent: 'gold',
  themeNeutral: 'mauve',
  publicNavigation: [
    { title: 'Domů', href: '/' },
    {
      title: 'Klub',
      children: [
        { title: 'O nás', href: '/o-nas' },
        { title: 'Kde trénujeme', href: '/kde-trenujeme' },
        { title: 'Trenéři', href: '/treneri' },
      ],
    },
    {
      title: 'Nabízíme',
      children: [
        { title: 'Vystoupení na akcích', href: '/vystoupeni' },
        { title: 'Školní taneční kroužky', href: '/skolni-krouzky' },
      ],
    },
    { title: 'Galerie', href: '/galerie' },
    { title: 'Akce', href: '/akce' },
    { title: 'Kontakt', href: '/kontakt' },
  ],
};
