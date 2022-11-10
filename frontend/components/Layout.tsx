import * as React from 'react';
import { Theme, useTheme, useMediaQuery } from '@mui/material';
import { DesktopHeader } from './DesktopHeader';
import { MobileHeader } from './MobileHeader';
import { Footer } from './Footer';
import { DesktopSidemenu } from './DesktopMenu';

export const Layout: React.FC = ({ children }) => {
  const theme = useTheme();
  const isTabletOrDesktop = useMediaQuery<Theme>(theme.breakpoints.up('md'));
  const Header = isTabletOrDesktop ? DesktopHeader : MobileHeader;
  return <>
    <Header />
    <div className="relative flex h-full min-h-screen w-full">
      {isTabletOrDesktop && <DesktopSidemenu />}
      <div className="flex flex-1">{children}</div>
    </div>
    <Footer />
  </>;
};
