const { toRadixVars } = require("windy-radix-palette/vars");

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    './pages/**/*.{js,ts,jsx,tsx}',
    './components/**/*.{js,ts,jsx,tsx}',
    "../@app/calendar/**/*.{js,ts,jsx,tsx}",
    "../@app/ui/**/*.{js,ts,jsx,tsx}",
    "../@app/upload/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      colors: {
        primary: '#ED1734',
        accent: toRadixVars('red'),
        neutral: toRadixVars('mauve'),
      },
    },
  },
  presets: [require('../tailwind-base.config.js')],
};
