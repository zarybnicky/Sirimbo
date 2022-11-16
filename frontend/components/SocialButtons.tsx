import * as React from 'react';
import { Instagram, Facebook, Youtube } from 'react-feather';
import Link from 'next/link';

type SocialButtonsProps = {
  variant: 'small' | 'medium' | 'large';
  className?: string;
};

export const SocialButtons = ({ className = "text-right" }: SocialButtonsProps) => {
  return <div className={className}>
    <Link href="https://www.facebook.com/tkolymp" passHref>
      <a className="button button-icon p-0 m-1" >
        <Facebook className="text-red-500" />
      </a>
    </Link>
    <Link href="https://www.instagram.com/tanecni_klub_olymp" passHref>
      <a className="button button-icon p-0 m-1" >
        <Instagram className="text-red-400" />
      </a>
    </Link>
    <Link href="https://www.youtube.com/user/TheMamcro" passHref>
      <a className="button button-icon p-0 m-1" >
        <Youtube className="text-gray-200" />
      </a>
    </Link>
  </div>
}
