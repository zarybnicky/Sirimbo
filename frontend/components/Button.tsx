import Link from 'next/link';
import React from 'react';

export const Button: React.FC<{
  href?: string;
  onClick?: React.MouseEventHandler<HTMLButtonElement>;
  type?: 'button' | 'submit' | 'reset';
}> = React.memo(function Button(props) {
  if (props.href) {
    return <Link href={props.href} passHref className="button button-rose">
      <a>{props.children}</a>
    </Link>
  }
  return <button type={props.type} onClick={props.onClick} className="button button-red">{props.children}</button>
})
