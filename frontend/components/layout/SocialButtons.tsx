import * as React from 'react';
import { Instagram, Facebook, Youtube } from 'react-feather';

type SocialButtonsProps = {
  variant: 'small' | 'medium' | 'large';
};

export const SocialButtons = ({ variant }: SocialButtonsProps) => {
  return (
    <div className="flex gap-2 items-center">
      <a
        target="_blank"
        rel="noreferrer"
        href="https://www.facebook.com/tkolymp"
        className="p-1"
      >
        <Facebook className={`text-red-600/90 ${variant === 'large' && 'w-10 h-10'}`} />
      </a>
      <a
        target="_blank"
        rel="noreferrer"
        href="https://www.instagram.com/tanecni_klub_olymp"
        className="p-1"
      >
        <Instagram className={`text-red-400 ${variant === 'large' && 'w-10 h-10'}`} />
      </a>
      <a
        target="_blank"
        rel="noreferrer"
        href="https://www.youtube.com/user/TheMamcro"
        className="p-1"
      >
        <Youtube className={`text-slate-100 ${variant === 'large' && 'w-10 h-10'}`} />
      </a>
    </div>
  );
};
