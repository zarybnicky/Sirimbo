const colors = require('tailwindcss/colors');

// https://www.tints.dev/red/ED1734
const red = {
  50: '#FDE7EA',
  100: '#FBD0D5',
  200: '#F8A0AC',
  300: '#F47687',
  400: '#F1465D',
  500: '#ED1734',
  600: '#C20F27',
  700: '#930B1D',
  800: '#5F0713',
  900: '#2F0409',
};

module.exports = {
  content: ['./pages/**/*.{js,ts,jsx,tsx}', './components/**/*.{js,ts,jsx,tsx}'],
  corePlugins: {
    aspectRatio: false,
  },
  plugins: [
    require('@tailwindcss/forms'),
    require('@tailwindcss/typography'),
    require('@tailwindcss/aspect-ratio'),
    require('@tailwindcss/line-clamp'),
    require('tailwindcss-opentype'),
    require('tailwindcss-radix')(),
    require('tailwind-scrollbar'),
    require('@headlessui/tailwindcss'),
  ],
  theme: {
    extend: {
      width: {
        fit: 'fit-content',
      },
      colors: {
        red,
        primary: red,
        secondary: colors.stone,

        success: '#90CA63',
        warning: '#e6be2a',
        error: '#e24320',
        message: '#90CA63',
      },
      gridColumn: {
        full: '1 / -1',
        content: 'content',
        feature: 'feature',
        popout: 'popout',
        'full-width': 'full',
      },
      backgroundImage: {
        'red-black-red':
          'linear-gradient(90deg, rgba(216,28,58,.8) 0%, rgba(0,0,0,0.8) 50%, rgba(216,28,58,.8) 100%)',
      },
    },
  },
};
