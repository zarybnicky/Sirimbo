import * as React from 'react';
import { Theme, useTheme, useMediaQuery } from '@material-ui/core';
import { DesktopHeader } from './DesktopHeader';
import { MobileHeader } from './MobileHeader';

export const Header = ({ }) => {
  const theme = useTheme();
  const isTabletOrDesktop = useMediaQuery<Theme>(theme.breakpoints.up('md'));
  return isTabletOrDesktop
    ? <DesktopHeader />
    : <MobileHeader />;
};
