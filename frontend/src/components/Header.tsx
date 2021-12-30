import * as React from 'react';
import { Theme, useTheme, useMediaQuery } from '@material-ui/core';
import { DesktopHeader } from './DesktopHeader';
import { MobileHeader } from './MobileHeader';
import { useMemberMenu, useMenu } from '../data/use-menu';

export const Header = ({ }) => {
  const theme = useTheme();
  const menu = useMenu();
  const memberMenu = useMemberMenu()
  const isTabletOrDesktop = useMediaQuery<Theme>(theme.breakpoints.up('md'));
  return isTabletOrDesktop
    ? <DesktopHeader {...{ menu, memberMenu }} />
    : <MobileHeader {...{ menu, memberMenu }} />;
};
