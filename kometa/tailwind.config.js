const { toRadixVars } = require("windy-radix-palette/vars");

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    './pages/**/*.{js,ts,jsx,tsx}',
    './components/**/*.{js,ts,jsx,tsx}',
    "../@app/ui/**/*.{js,ts,jsx,tsx}"
  ],
  theme: {
    extend: {
      colors: {
        primary: '#be9f69',
        accent: toRadixVars('gold'),
        neutral: toRadixVars('mauve'),
      },
    },
  },
  presets: [require('../tailwind-base.config.js')],
};
