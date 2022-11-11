import Link from 'next/link';
import React, { ButtonHTMLAttributes } from 'react';

export const Button: React.FC<{ href?: string; } & ButtonHTMLAttributes<HTMLButtonElement>> = React.memo(function Button(props) {
  if (props.href) {
    return <Link href={props.href} passHref className="button button-rose">
      <a>{props.children}</a>
    </Link>
  }
  return <button {...props} className="button button-red">{props.children}</button>
})
