import * as React from 'react';
import { Button, Menu, MenuItem } from '@mui/material';
import { PopupState as PopupStateType } from 'material-ui-popup-state/core';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { NestedMenuItem } from "./NestedMenuItem";
import { MenuStructItem, getHrefs, useTopMenu, useSideMenu } from 'lib/data/use-menu';
import { useRouter } from 'next/router';
import { NextLinkComposed } from './Link';
import KeyboardArrowDownIcon from '@mui/icons-material/KeyboardArrowDown';
import classNames from 'classnames';
import { MobileSubmenu } from './MobileHeader';

const Submenu = React.forwardRef<any, {
  popupState?: PopupStateType;
  item: MenuStructItem;
}>(function Submenu({ popupState, item: x }, ref) {
  const { pathname } = useRouter();
  const inPath = getHrefs(x).find(y => pathname.startsWith(y));

  if (x.type === 'link') {
    if (popupState) {
      return <MenuItem component={NextLinkComposed} href={x.href} onClick={popupState.close}>{x.text}</MenuItem>
    } else {
      return <Button
        component={NextLinkComposed} href={x.href}
        className="min-h-[48px] md:min-h-[64px] text-grey-500 font-bold rounded-none transition-colors"
        sx={[{
          "&:hover": {
            color: 'white',
            borderBottom: '3px solid white',
          },
        }, inPath ? {
          '&:not(:hover)': {
            color: 'white',
            borderBottom: '3px solid white',
          },
        } : {}]}>{x.text}</Button>;
    }
  }

  if (!popupState) {
    return <PopupState variant="popover" popupId={`menu-${x.text.replace(' ', '')} `}>
      {(popupState) => <>
        <Button
          {...bindTrigger(popupState)}
          color="inherit"
          endIcon={<KeyboardArrowDownIcon />}
          className="min-h-[48px] md:min-h-[64px] text-grey-500 font-bold rounded-none transition-colors"
          sx={[{
            "&:hover": {
              color: 'white',
              borderBottom: '3px solid white',
            },
          }, inPath ? {
            '&:not(:hover)': {
              color: 'white',
              borderBottom: '3px solid white',
            },
          } : {}]}
        >{x.text}</Button>
        <Menu
          {...bindMenu(popupState)}
          anchorOrigin={{ vertical: 'bottom', horizontal: 'left' }}
          transformOrigin={{ vertical: 'top', horizontal: 'left' }}
          sx={{
            '& .MuiMenu-paper': {
              backgroundColor: 'white',
              opacity: 1,
              borderRadius: 0,
            },
            '& .MuiListItem-button': {
              fontVariant: 'small-caps',
              display: 'flex',
            },
            '& .MuiListItem-button:hover': {
              color: '#d81c3a',
            },
          }}
        >
          {x.children.map(y => <Submenu key={y.text} popupState={popupState} item={y} />)}
        </Menu>
      </>}
    </PopupState>;
  } else {
    return (
      <NestedMenuItem label={x.text} parentMenuOpen={!!popupState.isOpen}>
        {x.children.map(y => <Submenu key={y.text} popupState={popupState} item={y} />)}
      </NestedMenuItem>
    );
  }
});

export const DesktopMenu = () => {
  const menu = useTopMenu();
  return <>{menu.map(x => <Submenu key={x.text} item={x} />)}</>;
}

export const DesktopSidemenu = () => {
  const menu = useSideMenu();
  if (menu.length < 1) {
    return null;
  }
  return (
    <nav className={classNames(
      'hidden md:block',
      `3xl:w-80 z-30 flex h-full max-h-screen min-h-screen w-3/4 flex-none transform flex-col overflow-y-auto border-r border-slate-200 bg-muted pb-10 sm:w-1/2 sm:pb-0 md:w-1/3 lg:relative lg:z-auto lg:w-56 lg:translate-x-0 lg:bg-gray-50 2xl:w-72`
    )}>
      {menu.map(x => <MobileSubmenu key={x.text} item={x} />)}
    </nav>
  );
}
