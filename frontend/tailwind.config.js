const colors = require("tailwindcss/colors");

const plugin = require("tailwindcss/plugin");
const typography = require("@material-tailwind/html/theme/base/typography");
const { button } = require("@material-tailwind/html/theme/components/button");

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
    require('tailwindcss-opentype'),
    require("tailwindcss-radix")(),
    require('tailwind-scrollbar'),
    plugin(function ({ addComponents, theme }) {
      addComponents(button(theme));
    }),
  ],
  theme: {
    fontFamily: typography,
    extend: {
      width: {
        fit: 'fit-content',
      },
      colors: {
        red: {
          50: '#fdf4f5',
          100: '#fbe8eb',
          200: '#f5c6ce',
          300: '#efa4b0',
          400: '#e46075',
          500: '#d81c3a',
          600: '#c21934',
          700: '#a2152c',
          800: '#821123',
          900: '#6a0e1c'
        },
        primary: {
          50: '#fdf4f5',
          100: '#fbe8eb',
          200: '#f5c6ce',
          300: '#efa4b0',
          400: '#e46075',
          500: '#d81c3a',
          600: '#c21934',
          700: '#a2152c',
          800: '#821123',
          900: '#6a0e1c'
        },
        'blue-gray': colors.slate,
        secondary: colors.stone,

        text: "#575757",

        success: '#90CA63',
        warning: '#e6be2a',
        error: '#e24320',
        message: '#90CA63',
      },
      gridColumn: {
        'full': '1 / -1',
      },
      backgroundImage: {
        'red-black-red': 'linear-gradient(90deg, rgba(216,28,58,.8) 0%, rgba(0,0,0,0.8) 50%, rgba(216,28,58,.8) 100%)'
      },
    },
  },
};
