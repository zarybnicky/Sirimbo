import React from 'react';
import classNames from 'classnames';
import Link from 'next/link';
import { Menu, Transition } from '@headlessui/react';
import { MoreVertical } from 'react-feather';

const MenuLink = React.forwardRef<HTMLAnchorElement, React.HTMLProps<HTMLAnchorElement>>(
  function MenuLink({ href, children, ...rest }, ref) {
    return (
      <Link href={href as string} passHref>
        <a ref={ref} {...rest}>
          {children}
        </a>
      </Link>
    );
  },
);

const defaultDropdown = (
  <MoreVertical className="text-stone-500 w-6 invisible ui-open:visible group-hover:visible" />
);

export type DropdownItem = {
  icon?: React.ReactNode;
  title: string;
  href?: string;
  onClick?: () => void;
};

export const Dropdown = ({
  align,
  button = defaultDropdown,
  className,
  buttonClassName,
  options,
}: {
  button?: React.ReactNode;
  className?: string;
  buttonClassName?: string;
  options: DropdownItem[];
  align: 'start' | 'end' | 'center';
}) => {
  return (
    <Menu
      as="div"
      className={classNames(!className?.includes('absolute') && 'relative', className)}
    >
      <Menu.Button className={buttonClassName}>{button}</Menu.Button>
      <Transition
        as={React.Fragment}
        enter="transition ease-out duration-200"
        enterFrom="transform opacity-0 scale-95"
        enterTo="transform opacity-100 scale-100"
        leave="transition ease-in duration-75"
        leaveFrom="transform opacity-100 scale-100"
        leaveTo="transform opacity-0 scale-95"
      >
        <Menu.Items
          className={classNames(
            'z-[1100] absolute w-48 rounded-md shadow-lg py-1 bg-white ring-1 ring-black ring-opacity-5 focus:outline-none',
            align === 'center'
              ? 'origin-top-right right-1/2 transform translate-x-1/2'
              : align === 'start'
              ? 'origin-top-left left-0'
              : 'origin-top-right right-0',
          )}
        >
          {options.map(({ title, icon, href, onClick }, i) => (
            <Menu.Item key={i}>
              {({ active }) =>
                href ? (
                  <MenuLink
                    href={href}
                    className={classNames(
                      active ? 'bg-gray-100' : '',
                      'block px-4 py-2 text-sm text-gray-800',
                    )}
                  >
                    {icon} {title}
                  </MenuLink>
                ) : (
                  <button
                    onClick={onClick}
                    className={classNames(
                      active ? 'bg-gray-100' : '',
                      'text-left w-full block px-4 py-2 text-sm text-gray-800',
                    )}
                  >
                    {icon} {title}
                  </button>
                )
              }
            </Menu.Item>
          ))}
        </Menu.Items>
      </Transition>
    </Menu>
  );
};
