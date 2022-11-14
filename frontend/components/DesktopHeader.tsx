import * as React from 'react';
import Link from 'next/link';
import { AppBar, Toolbar } from '@mui/material';
import { SocialButtons } from './SocialButtons';
import { AuthButton } from './AuthButton';
import { DesktopMenu } from './DesktopMenu';
import { OlympLogoVertical } from 'components/Icons';

export const DesktopHeader = ({ }) => {
  return <AppBar position="static" color="secondary" className="hidden md:block">
    <Toolbar className="bg-stone-800">
      <div className="container mx-auto max-w-5xl flex items-center justify-between">
        <div className="relative overflow-visible min-w-[104px] min-h-[48px] md:min-h-[64px]">
          <div className="w-[104px] h-[130px] text-white bg-red-500 z-50 shadow-red-900/40 shadow-lg absolute top-0 left-0 right-0">
            <Link passHref href="/" className="block p-0 m-0 h-full w-full relative">
              <a>
                <OlympLogoVertical style={{
                  filter: 'drop-shadow(0px 6px 6px rgba(0, 0, 0, 0.2))',
                  position: 'absolute',
                  left: 0,
                  bottom: 0,
                  width: '104px',
                  height: '104px',
                  color: 'white',
                  fill: 'white !important',
                }} />
              </a>
            </Link>
          </div>
        </div>
        <DesktopMenu />
        <SocialButtons variant="medium" />
        <AuthButton />
      </div>
    </Toolbar>
  </AppBar >;
};
