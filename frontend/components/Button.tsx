import Link from 'next/link';
import { Route } from 'nextjs-routes';
import React, { ButtonHTMLAttributes } from 'react';

type ButtonProps = {
  href?: Route | Exclude<Route, { query: any }>["pathname"] | Omit<Route, "pathname">;
} & ButtonHTMLAttributes<HTMLButtonElement>;

export const Button = React.memo(function Button({
  href,
  type = 'submit',
  children,
  ...props
}: ButtonProps) {
  const color = props.disabled
    ? 'button text-center button-secondary'
    : 'button text-center button-red';
  if (href) {
    return (
      <Link href={href} className={color}>
        {children}
      </Link>
    );
  }
  return (
    <button type={type} {...props} className={color}>
      {children}
    </button>
  );
});