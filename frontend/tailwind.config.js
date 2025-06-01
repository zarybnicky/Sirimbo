const radixColors = require("@radix-ui/colors");
const { toRadixVars } = require("windy-radix-palette/vars");
const { tenantConfig: tenant } = require('./tenant/config.js');

/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    './**/*.{js,ts,jsx,tsx}',
    "../../libs/**/*.{js,ts,jsx,tsx}",
  ],
  darkMode: 'media',
  corePlugins: {
    aspectRatio: false,
  },
  plugins: [
    require("windy-radix-palette")({
      colors: {
        green: radixColors.green,
        greenDark: radixColors.greenDark,
        [tenant.themeNeutral]: tenant.neutralLight || radixColors[tenant.themeNeutral],
        [`${tenant.themeNeutral}Dark`]: tenant.neutralDark || radixColors[`${tenant.themeNeutral}Dark`],
        [tenant.themeAccent]: tenant.accentLight || radixColors[tenant.themeAccent],
        [`${tenant.themeAccent}Dark`]: tenant.accentDark || radixColors[`${tenant.themeAccent}Dark`],
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
    colors: {
      auto: 'auto',
      inherit: 'inherit',
      current: 'current',
      transparent: 'transparent',
      black: '#000',
      white: '#fff',
      primary: tenant.themePrimary,
      accent: toRadixVars(tenant.themeAccent),
      neutral: toRadixVars(tenant.themeNeutral),
      green: toRadixVars('green'),
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
