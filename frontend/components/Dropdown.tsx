import React from 'react';
import classNames from 'classnames';
import * as DropdownMenuPrimitive from "@radix-ui/react-dropdown-menu";
import Link from 'next/link';

export const Dropdown = ({ button, options }: {
  button: React.ReactNode;
  options: {
    icon?: React.ReactNode;
    title: string;
    href?: string;
    onClick?: () => void;
  }[];
}) => {
  return (
    <DropdownMenuPrimitive.Root>
      <DropdownMenuPrimitive.Trigger asChild>
        {button}
      </DropdownMenuPrimitive.Trigger>

      <DropdownMenuPrimitive.Portal>
        <DropdownMenuPrimitive.Content
          align="center"
          sideOffset={5}
          className={classNames(
            " radix-side-top:animate-slide-up radix-side-bottom:animate-slide-down",
            "w-48 rounded-lg px-1.5 py-1 shadow-md md:w-56",
            "bg-white dark:bg-gray-800"
          )}
        >
          {options.map(({ title, icon, href, onClick }, i) => (
            <DropdownMenuPrimitive.Item asChild key={`${title}-${i}`}>
              {href ? (
                <Link href={href} passHref>
                  <a
                    className={classNames(
                      "flex cursor-default select-none items-center rounded-md px-2 py-2 text-xs outline-none",
                      "text-gray-400 focus:bg-gray-50 dark:text-gray-500 dark:focus:bg-gray-900"
                    )}
                  >
                    {icon}
                    <span className="flex-grow text-gray-700 dark:text-gray-300">
                      {title}
                    </span>
                  </a>
                </Link>
              ) : (
                <button
                  onClick={onClick}
                  className={classNames(
                    "flex cursor-default select-none items-center rounded-md px-2 py-2 text-xs outline-none",
                    "text-gray-400 focus:bg-gray-50 dark:text-gray-500 dark:focus:bg-gray-900"
                  )}
                >
                  {icon}
                  <span className="flex-grow text-gray-700 dark:text-gray-300">
                    {title}
                  </span>
                </button>
              )}
            </DropdownMenuPrimitive.Item>
          ))}
        </DropdownMenuPrimitive.Content>
      </DropdownMenuPrimitive.Portal>
    </DropdownMenuPrimitive.Root>
  );
};

const Button = ({ children, ...props }: Omit<React.ComponentProps<"button">, "className">) => (
  <button
    {...props}
    className={classNames(
      "inline-flex select-none items-center justify-center rounded-md px-4 py-2 text-sm font-medium",
      "bg-white text-gray-700 hover:bg-gray-50 dark:bg-gray-800 dark:text-gray-100 dark:hover:bg-gray-900",
      "hover:bg-gray-50",
      "focus:outline-none focus-visible:ring focus-visible:ring-red-500 focus-visible:ring-opacity-75",
      "group",
      "radix-state-open:bg-gray-50 dark:radix-state-open:bg-gray-900",
      "radix-state-on:bg-gray-50 dark:radix-state-on:bg-gray-900",
      "radix-state-instant-open:bg-gray-50 radix-state-delayed-open:bg-gray-50"
    )}
  >
    {children}
  </button>
);
