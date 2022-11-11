const colors = require("tailwindcss/colors");

const plugin = require("tailwindcss/plugin");
const typography = require("@material-tailwind/html/theme/base/typography");
const { accordion } = require("@material-tailwind/html/theme/components/accordion");
const { alert } = require("@material-tailwind/html/theme/components/alert");
const { avatar } = require("@material-tailwind/html/theme/components/avatar");
const { background } = require("@material-tailwind/html/theme/components/background");
const { breadcrumbs } = require("@material-tailwind/html/theme/components/breadcrumbs");
const { button } = require("@material-tailwind/html/theme/components/button");
const { card } = require("@material-tailwind/html/theme/components/card");
const { carousel } = require("@material-tailwind/html/theme/components/carousel");
const { checkbox } = require("@material-tailwind/html/theme/components/checkbox");
const { chip } = require("@material-tailwind/html/theme/components/chip");
const { datepicker } = require("@material-tailwind/html/theme/components/datepicker");
const { dialog } = require("@material-tailwind/html/theme/components/dialog");
const { hr } = require("@material-tailwind/html/theme/components/hr");
const { input } = require("@material-tailwind/html/theme/components/input");
const { menu } = require("@material-tailwind/html/theme/components/menu");
const { navbar } = require("@material-tailwind/html/theme/components/navbar");
const { notifications } = require("@material-tailwind/html/theme/components/notifications");
const { pagination } = require("@material-tailwind/html/theme/components/pagination");
const { popover } = require("@material-tailwind/html/theme/components/popover");
const { progress } = require("@material-tailwind/html/theme/components/progress");
const { radio } = require("@material-tailwind/html/theme/components/radio");
const { range } = require("@material-tailwind/html/theme/components/range");
const { spinners } = require("@material-tailwind/html/theme/components/spinners");
const { tabs } = require("@material-tailwind/html/theme/components/tabs");
const { toggle } = require("@material-tailwind/html/theme/components/switch");
const { tooltip } = require("@material-tailwind/html/theme/components/tooltip");
const { typo } = require("@material-tailwind/html/theme/components/typo");

module.exports = {
  content: [
    "./pages/**/*.{js,ts,jsx,tsx}",
    "./components/**/*.{js,ts,jsx,tsx}",
  ],
  corePlugins: {
    aspectRatio: false,
  },
  plugins: [
    require("@tailwindcss/forms"),
    require("@tailwindcss/typography"),
    require("@tailwindcss/aspect-ratio"),
    require('@tailwindcss/line-clamp'),
    plugin(function ({ addComponents, theme }) {
      addComponents(accordion(theme));
      addComponents(alert(theme));
      addComponents(avatar(theme));
      addComponents(background(theme));
      addComponents(breadcrumbs(theme));
      addComponents(button(theme));
      addComponents(card(theme));
      addComponents(carousel(theme));
      addComponents(checkbox(theme));
      addComponents(chip(theme));
      addComponents(dialog(theme));
      addComponents(datepicker(theme));
      addComponents(hr(theme));
      addComponents(input(theme));
      addComponents(menu(theme));
      addComponents(navbar(theme));
      addComponents(notifications(theme));
      addComponents(pagination(theme));
      addComponents(popover(theme));
      addComponents(progress(theme));
      addComponents(range(theme));
      addComponents(radio(theme));
      addComponents(spinners(theme));
      addComponents(tabs(theme));
      addComponents(toggle(theme));
      addComponents(tooltip(theme));
      addComponents(typo(theme));
    }),
  ],
  theme: {
    fontFamily: typography,
    extend: {
      width: {
        fit: 'fit-content',
      },
      colors: {
        'blue-gray': colors.slate,
        primary: '#d81c3a',
        secondary: '#222222',

        text: "#575757",

        success: '#90CA63',
        warning: '#e6be2a',
        error: '#e24320',
        message: '#90CA63',
      },
      backgroundImage: {
        'red-black-red': 'linear-gradient(90deg, rgba(216,28,58,.8) 0%, rgba(0,0,0,0.8) 50%, rgba(216,28,58,.8) 100%)'
      },
    },
  },
};
