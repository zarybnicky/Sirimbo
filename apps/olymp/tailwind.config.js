const { toRadixVars } = require("windy-radix-palette/vars");

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    './pages/**/*.{js,ts,jsx,tsx}',
    './components/**/*.{js,ts,jsx,tsx}',
    "../../libs/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      colors: {
        primary: process.env.THEME_PRIMARY,
        accent: toRadixVars(process.env.THEME_ACCENT),
        neutral: toRadixVars(process.env.THEME_NEUTRAl),
      },
    },
  },
  presets: [require('../../tailwind-base.config.js')],
};
