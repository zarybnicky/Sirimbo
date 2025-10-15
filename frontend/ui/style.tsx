import { tv } from 'tailwind-variants';

export const buttonCls = tv({
  base: 'relative appearance-none focus:outline-none disabled:cursor-not-allowed transition-colors',
  variants: {
    display: {
      none: ' ',
      button: 'inline-flex gap-1 shadow-md uppercase font-medium justify-center items-center',
      iconOnly: 'inline-flex items-center justify-center',
      listItem: [
        'group w-full text-left flex items-center gap-2',
        'px-2.5 py-2 text-sm first:rounded-t-xl border last:rounded-b-xl',
        'bg-neutral-1 text-accent-11 border-y border-l last:border-r border-accent-7',
        'data-[state=on]:text-white data-[state=on]:bg-accent-9 data-[state=on]:border-accent-10',
        'disabled:text-neutral-11 disabled:border-neutral-6',
        'disabled:data-[state=on]:text-white disabled:data-[state=on]:bg-neutral-9 disabled:data-[state=on]:border-neutral-10',
        'focus-visible:z-30 focus-visible:ring focus-visible:ring-accent-10',
      ].join(' '),
    },
    variant: {
      none: ' ',
      primary: 'bg-accent-9 hover:bg-accent-10 active:bg-accent-10 text-accent-0 disabled:bg-neutral-3 disabled:text-neutral-11',
      outline: 'bg-accent-2 hover:bg-accent-3 active:bg-accent-5 text-accent-12 border border-accent-6 hover:border-accent-7',
      outlineDark: 'bg-neutral-12 text-neutral-0',
      subtle: 'bg-transparent text-accent-9 hover:text-accent-10 disabled:text-accent-7 disabled:hover:text-accent-7',
    },
    size: {
      none: 'p-0 [&_svg]:w-5 [&_svg]:h-5',
      xs: 'px-1.5 py-1 text-xs rounded-xl tracking-tight [&_svg]:w-3 [&_svg]:h-3',
      sm: 'px-2 py-1.5 text-xs rounded-xl tracking-tight [&_svg]:w-3 [&_svg]:h-3',
      md: 'px-3 py-2 text-sm rounded-xl [&_svg]:w-4 [&_svg]:h-4',
      lg: 'px-6 py-3 text-base rounded-xl [&_svg]:w-5 [&_svg]:h-5',
      icon: 'p-0 leading-none [&_svg]:w-5 [&_svg]:h-5',
    },
  },
  defaultVariants: {
    display: 'button',
    variant: 'primary',
    size: 'md',
  },
});

export const cardCls = tv({
  base: 'group bg-neutral-1 relative border border-neutral-6 shadow-sm sm:rounded-lg p-3 mb-1',
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
    spacing: {
      default: '',
      topLevel: 'mt-12',
    },
  },
  defaultVariants: {
    variant: 'heading',
    spacing: 'default',
  },
});
