import React from 'react';
import cx from 'classnames';
import Link from 'next/link';
import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import { Route } from 'nextjs-routes';

export type DropdownItem = {
  icon?: React.ReactNode;
  title: string;
  href?: Route | Exclude<Route, { query: any }>['pathname'] | Omit<Route, 'pathname'>;
  onClick?: () => void;
};

type DropdownProps = {
  button: React.ReactNode;
  options: DropdownItem[];
  align: 'start' | 'end' | 'center';
  modal?: boolean;
};

export function Dropdown({ align, button, options, modal = true }: DropdownProps) {
  return (
    <DropdownMenu.Root modal={modal}>
      <DropdownMenu.Trigger asChild>{button}</DropdownMenu.Trigger>

      <DropdownMenu.Portal>
        <DropdownMenu.Content
          align={align}
          sideOffset={5}
          className={cx(
            'min-w-[220px] bg-white rounded-md p-[5px]',
            'shadow-[0px_10px_38px_-10px_rgba(22,_23,_24,_0.35),_0px_10px_20px_-15px_rgba(22,_23,_24,_0.2)] will-change-[opacity,transform]',
            'data-[side=top]:animate-slideDownAndFade data-[side=right]:animate-slideLeftAndFade data-[side=bottom]:animate-slideUpAndFade data-[side=left]:animate-slideRightAndFade',
          )}
        >
          {options.map(({ href, icon, title, onClick }, i) => {
            const classes = cx(
              'group text-sm leading-none text-red-500 rounded-[3px] flex items-center p-3 relative pl-[25px]',
              'select-none outline-none',
              'data-[disabled]:text-stone-800 data-[disabled]:pointer-events-none cursor-pointer',
              'data-[highlighted]:bg-red-500 data-[highlighted]:text-red-50 data-[highlighted]:font-bold',
            );
            return (
              <DropdownMenu.Item asChild key={i}>
                {href ? (
                  <Link className={classes} href={href}>
                    {icon} {title}
                  </Link>
                ) : (
                  <div className={classes} onClick={onClick}>
                    {icon} {title}
                  </div>
                )}
              </DropdownMenu.Item>
            );
          })}

          <DropdownMenu.Arrow className="fill-white" />
        </DropdownMenu.Content>
      </DropdownMenu.Portal>
    </DropdownMenu.Root>
  );
}
