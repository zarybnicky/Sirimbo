import Link from 'next/link';
import React, { ButtonHTMLAttributes } from 'react';

type ButtonProps = { href?: string; } & ButtonHTMLAttributes<HTMLButtonElement>;

export const Button = React.memo(function Button({
  href, type = "submit", children, ...props
}: ButtonProps) {
  const color = props.disabled ? 'button button-secondary' : 'button button-red';
  if (href) {
    return <Link href={href} passHref className={color}>
      <a>{children}</a>
    </Link>
  }
  return <button type={type} {...props} className={color}>{children}</button>
})
