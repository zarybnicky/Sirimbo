const radixColors = require("@radix-ui/colors");
const { toRadixVars } = require("windy-radix-palette/vars");

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    './src/**/*.{js,ts,jsx,tsx}'
  ],
  darkMode: 'media',
  corePlugins: {
    aspectRatio: false,
  },
  plugins: [
		require("windy-radix-palette")({
			colors: {
				mauve: radixColors.mauve,
				mauveDark: radixColors.mauveDark,
				red: radixColors.red,
				redDark: radixColors.redDark,
				gold: radixColors.gold,
				goldDark: radixColors.goldDark,
			},
		}),
    require('@tailwindcss/forms'),
    require('@tailwindcss/typography'),
		require("windy-radix-typography")({
      colors: ['accent', 'neutral'],
    }),
    require('@tailwindcss/aspect-ratio'),
    require('tailwindcss-opentype'),
    require('tailwind-scrollbar'),
  ],
  theme: {
    container: {
      center: true,
      padding: {
        DEFAULT: '.5rem',
        md: '2rem',
      },
    },
    extend: {
      width: {
        fit: 'fit-content',
      },
      colors: {
        primary: '#ED1734',
        accent: toRadixVars('red'),
        neutral: toRadixVars('mauve'),
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
      keyframes: {
        slideDownAndFade: {
          from: { opacity: 0, transform: 'translateY(-2px)' },
          to: { opacity: 1, transform: 'translateY(0)' },
        },
        slideLeftAndFade: {
          from: { opacity: 0, transform: 'translateX(2px)' },
          to: { opacity: 1, transform: 'translateX(0)' },
        },
        slideUpAndFade: {
          from: { opacity: 0, transform: 'translateY(2px)' },
          to: { opacity: 1, transform: 'translateY(0)' },
        },
        slideRightAndFade: {
          from: { opacity: 0, transform: 'translateX(-2px)' },
          to: { opacity: 1, transform: 'translateX(0)' },
        },
        overlayShow: {
          from: { opacity: '0' },
          to: { opacity: '1' },
        },
        overlayHide: {
          from: { opacity: '1' },
          to: { opacity: '0' },
        },
        contentShow: {
          from: { opacity: '0', transform: 'translate(-50%, -48%) scale(0.96)' },
          to: { opacity: '1', transform: 'translate(-50%, -50%) scale(1)' },
        },
        contentHide: {
          from: { opacity: '1', transform: 'translate(-50%, -50%) scale(1)' },
          to: { opacity: '0', transform: 'translate(-50%, -48%) scale(0.96)' },
        },
      },
      animation: {
        slideDownAndFade: 'slideDownAndFade 400ms cubic-bezier(0.16, 1, 0.3, 1)',
        slideLeftAndFade: 'slideLeftAndFade 400ms cubic-bezier(0.16, 1, 0.3, 1)',
        slideUpAndFade: 'slideUpAndFade 400ms cubic-bezier(0.16, 1, 0.3, 1)',
        slideRightAndFade: 'slideRightAndFade 400ms cubic-bezier(0.16, 1, 0.3, 1)',
        overlayShow: 'overlayShow 350ms cubic-bezier(0.16, 1, 0.3, 1)',
        contentShow: 'contentShow 350ms cubic-bezier(0.16, 1, 0.3, 1)',
        overlayHide: 'overlayHide 350ms cubic-bezier(0.16, 1, 0.3, 1)',
        contentHide: 'contentHide 350ms cubic-bezier(0.16, 1, 0.3, 1)',
      },
    },
  },
};
