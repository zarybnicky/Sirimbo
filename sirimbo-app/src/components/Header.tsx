import * as React from 'react';
import { Theme, useTheme, useMediaQuery } from '@material-ui/core';
import { DesktopHeader } from './DesktopHeader';
import { MobileHeader } from './MobileHeader';
import { useAuth } from '../use-auth';
import { useMenu } from '../use-menu';

export const Header = ({ }) => {
  const theme = useTheme();
  const menu = useMenu();
  const auth = useAuth();
  const isTabletOrDesktop = useMediaQuery<Theme>(theme.breakpoints.up('md'));
  return isTabletOrDesktop
    ? <DesktopHeader {...{ menu, auth }} />
    : <MobileHeader {...{ menu, auth }} />;
};
