import * as React from 'react';
import { Theme, useTheme, useMediaQuery } from '@mui/material';
import { DesktopHeader } from './DesktopHeader';
import { MobileHeader } from './MobileHeader';
import { Footer } from './Footer';

export const NewLayout: React.FC = ({ children }) => {
  const theme = useTheme();
  const isTabletOrDesktop = useMediaQuery<Theme>(theme.breakpoints.up('md'));
  const Header = isTabletOrDesktop ? DesktopHeader : MobileHeader;
  return <>
    <Header />
    {children}
    <Footer />
  </>;
};
