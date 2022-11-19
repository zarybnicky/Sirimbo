import * as React from 'react';
import { Instagram, Facebook, Youtube } from 'react-feather';
import Link from 'next/link';

type SocialButtonsProps = {
  variant: 'small' | 'medium' | 'large';
  className?: string;
};

export const SocialButtons = ({ variant }: SocialButtonsProps) => {
  return <div className="flex gap-3 items-center">
    <Link href="https://www.facebook.com/tkolymp" passHref>
      <a className="p-1">
        <Facebook className={`text-red-500 ${variant === "large" && 'w-10 h-10'}`} />
      </a>
    </Link>
    <Link href="https://www.instagram.com/tanecni_klub_olymp" passHref>
      <a className="p-1" >
        <Instagram className={`text-red-400 ${variant === "large" && 'w-10 h-10'}`} />
      </a>
    </Link>
    <Link href="https://www.youtube.com/user/TheMamcro" passHref>
      <a className="p-1">
        <Youtube className={`text-gray-200 ${variant === "large" && 'w-10 h-10'}`} />
      </a>
    </Link>
  </div>
}
