/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    './calendar/**/*.{js,ts,jsx,tsx}',
    './lib/**/*.{js,ts,jsx,tsx}',
    './pages/**/*.{js,ts,jsx,tsx}',
    './scoreboard/**/*.{js,ts,jsx,tsx}',
    './starlet/**/*.{js,ts,jsx,tsx}',
    './style/**/*.{js,ts,jsx,tsx}',
    './tenant/**/*.{js,ts,jsx,tsx}',
    './ui/**/*.{js,ts,jsx,tsx}',
  ],
  darkMode: 'media',
  corePlugins: {
    aspectRatio: false,
  },
  plugins: [
    require('@tailwindcss/forms'),
    require('@tailwindcss/typography'),
    require("windy-radix-typography")({
      colors: ['accent', 'neutral'],
    }),
    require('@tailwindcss/aspect-ratio'),
    require('tailwindcss-opentype'),
    require('tailwind-scrollbar'),
  ],
  safelist: [
    ...Array({ length: 10 }).keys().map(i => `tenant-${i}`),
  ],
  theme: {
    container: {
      center: true,
      padding: {
        DEFAULT: '.5rem',
        md: '2rem',
      },
    },
    colors: {
      auto: 'auto',
      inherit: 'inherit',
      current: 'current',
      transparent: 'transparent',
      black: '#000',
      white: '#fff',
      accent: Object.fromEntries(
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12].map(
          (n) => [n, `hsl(var(--accent-${n}) / <alpha-value>)`]
        ),
      ),
      neutral: Object.fromEntries(
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12].map(
          (n) => [n, `hsl(var(--neutral-${n}) / <alpha-value>)`]
        ),
      ),
      green: Object.fromEntries(
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12].map(
          (n) => [n, `hsl(var(--green-${n}) / <alpha-value>)`]
        ),
      ),
    },
    extend: {
      width: {
        fit: 'fit-content',
      },
      height: {
        fit: 'fit-content',
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
          from: { opacity: '0', transform: 'scale(0.96)' },
          to: { opacity: '1', transform: 'scale(1)' },
        },
        contentHide: {
          from: { opacity: '1', transform: 'scale(1)' },
          to: { opacity: '0', transform: 'scale(0.96)' },
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
