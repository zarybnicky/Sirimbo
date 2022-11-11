import * as React from 'react';
import { IconButton } from '@mui/material';
import InstagramIcon from '@mui/icons-material/Instagram';
import FacebookIcon from '@mui/icons-material/Facebook';
import YouTubeIcon from '@mui/icons-material/YouTube';

type SocialButtonsProps = {
  variant: 'small' | 'medium' | 'large';
  className?: string;
};

export const SocialButtons = ({ variant = 'medium', className = "text-right" }: SocialButtonsProps) => {
  return <div className={className}>
    <IconButton className="p-0 m-1" href="https://www.facebook.com/tkolymp">
      <FacebookIcon className="text-red-500" fontSize={variant} />
    </IconButton>
    <IconButton className="p-0 m-1" href="https://www.instagram.com/tanecni_klub_olymp">
      <InstagramIcon className="text-red-400" fontSize={variant} />
    </IconButton>
    <IconButton className="p-0 m-1" href="https://www.youtube.com/user/TheMamcro">
      <YouTubeIcon className="text-gray-200" fontSize={variant} />
    </IconButton>
  </div>
}
