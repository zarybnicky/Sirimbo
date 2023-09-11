import { tv } from 'tailwind-variants';

export const buttonCls = tv({
  base: 'relative appearance-none',
  variants: {
    display: {
      none: ' ',
      button: 'inline-flex gap-1 shadow-md uppercase font-medium justify-center items-center',
      listItem: 'flex flex-col gap-1 shadow-sm font-medium items-start mb-0.5',
    },
    variant: {
      none: ' ',
      primary: 'bg-accent-9 hover:bg-accent-10 active:bg-accent-10 text-accent-0 disabled:bg-neutral-3 disabled:text-neutral-11',
      outline: 'bg-accent-2 hover:bg-accent-3 active:bg-accent-5 text-accent-12 border border-accent-6 hover:border-accent-7',
      outlineDark: 'bg-neutral-12 text-neutral-0',
    },
    size: {
      sm: 'px-2 py-1.5 text-xs rounded-xl tracking-tight [&_svg]:w-3 [&_svg]:h-3',
      md: 'px-3 py-2 text-sm rounded-xl [&_svg]:w-4 [&_svg]:h-4',
      lg: 'px-6 py-3 text-base rounded-xl [&_svg]:w-5 [&_svg]:h-5',
    },
  },
  defaultVariants: {
    display: 'button',
    variant: 'primary',
    size: 'md',
  },
});

export const buttonGroupCls = tv({
  base: 'inline-flex rounded-xl shadow-md [&_button]:rounded-none [&_button]:shadow-none [&_button:first-child]:rounded-l-xl [&_button:last-child]:rounded-r-xl',
});

export const typographyCls = tv({
  base: '',
  variants: {
    variant: {
      heading: 'text-4xl text-accent-12 drop-shadow tracking-wide',
      smallHeading: 'text-2xl text-neutral-12 drop-shadow tracking-wide',
      section: 'text-3xl text-accent-11 drop-shadow tracking-wide',
      label: 'block text-sm text-neutral-11 mt-1',
    },
  },
  defaultVariants: {
    variant: 'heading',
  },
});
