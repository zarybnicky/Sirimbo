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
  return <div className="h-screen w-full overflow-hidden">
    <Header />
    <div className="relative h-full flex">
      {isTabletOrDesktop && <DesktopSidemenu />}
      <div className="flex flex-1 flex-col overflow-y-auto">
        {children}
        <Footer />
      </div>
    </div>
  </div>;
};
