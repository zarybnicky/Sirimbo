import { tv } from 'tailwind-variants';

export const buttonCls = tv({
  base: 'relative appearance-none focus:outline-hidden focus-visible:ring-3 focus-visible:ring-accent-10',
  variants: {
    display: {
      none: ' ',
      button:
        'inline-flex gap-1 shadow-md uppercase font-medium justify-center items-center',
      listItem:
        'flex w-full items-center px-2.5 py-2 text-left hover:bg-accent-3 active:bg-accent-5',
    },
    variant: {
      none: ' ',
      primary:
        'bg-accent-9 hover:bg-accent-10 active:bg-accent-10 text-accent-0 disabled:bg-neutral-3 disabled:text-neutral-11',
      outline:
        'bg-accent-2 hover:bg-accent-3 active:bg-accent-5 text-accent-12 border border-accent-6 hover:border-accent-7 disabled:bg-neutral-2 disabled:text-neutral-9 disabled:border-neutral-5',
      outlineDark: 'bg-neutral-12 text-neutral-0',
    },
    size: {
      none: ' ',
      icon: 'size-8 rounded-xl [&_svg]:size-4',
      xs: 'px-1.5 py-1 text-xs rounded-xl tracking-tight [&_svg]:size-3',
      sm: 'px-2 py-1.5 text-xs rounded-xl tracking-tight [&_svg]:size-3',
      md: 'px-3 py-2 text-sm rounded-xl [&_svg]:size-4',
      lg: 'px-6 py-3 text-base rounded-xl [&_svg]:size-5',
    },
  },
  defaultVariants: {
    display: 'button',
    variant: 'primary',
    size: 'md',
  },
});

export const cardCls = tv({
  base: 'group bg-neutral-1 relative border border-neutral-6 shadow-xs sm:rounded-lg p-3 mb-1',
});

export const buttonGroupCls = tv({
  base: 'inline-flex rounded-xl shadow-md [&_button]:rounded-none [&_button]:shadow-none [&_button:first-child]:rounded-l-xl [&_button:last-child]:rounded-r-xl',
});

export const badgeCls = tv({
  base: 'inline-flex items-center rounded-full px-2 py-0.5 text-[11px] uppercase',
  variants: {
    variant: {
      neutral:
        'border border-neutral-7 bg-neutral-2 font-medium tracking-wide text-neutral-11',
      accent: 'bg-accent-3 tracking-tight text-accent-11',
    },
  },
  defaultVariants: { variant: 'neutral' },
});

export const checkboxInputCls = tv({
  base: 'size-4 rounded-sm border-2 border-accent-9 bg-accent-2 text-accent-10 checked:border-transparent checked:bg-accent-10 focus:ring-accent-9',
});

export const typographyCls = tv({
  base: '',
  variants: {
    variant: {
      heading: 'text-3xl md:text-4xl text-accent-12 drop-shadow-sm tracking-wide',
      smallHeading: 'text-xl md:text-2xl text-neutral-12 drop-shadow-sm tracking-wide',
      cardHeading: 'text-lg font-bold text-accent-12',
      section: 'text-3xl text-accent-11 drop-shadow-sm tracking-wide',
      label: 'block text-sm text-neutral-11 mt-1',
    },
    spacing: {
      default: '',
      topLevel: 'mt-8',
    },
  },
  defaultVariants: {
    variant: 'heading',
    spacing: 'default',
  },
});
