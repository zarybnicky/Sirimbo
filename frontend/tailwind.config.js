const defaultTheme = require('tailwindcss/defaultTheme');
const colors = require('tailwindcss/colors');

module.exports = {
  mode: "jit",
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
  ],
  theme: {
    extend: {
      width: {
        fit: 'fit-content',
      },
      colors: {
        primary: '#d81c3a',
        secondary: '#222222',

        text: "#575757",

        gray: '#9a9a9a',
        muted: '#f1f1f1',
        success: '#90CA63',
        warning: '#e6be2a',
        error: '#e24320',
        message: '#90CA63',

        separator: '#eaeaea',
        highlight: '#f1f1f1',
        highlightHigh: '#f7f7f7',
      },
    },
  },
};
