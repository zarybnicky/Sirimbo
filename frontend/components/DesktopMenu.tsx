import * as React from 'react';
import { alpha, Button, Menu, MenuItem } from '@mui/material';
import { useTheme } from '@mui/material/styles';
import { PopupState as PopupStateType } from 'material-ui-popup-state/core';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { NestedMenuItem } from "./NestedMenuItem";
import { useMenu, MenuStructItem, getHrefs } from 'lib/data/use-menu';
import { useRouter } from 'next/router';
import { NextLinkComposed } from './Link';
import KeyboardArrowDownIcon from '@mui/icons-material/KeyboardArrowDown';

const Submenu = React.forwardRef<any, {
  popupState?: PopupStateType;
  item: MenuStructItem;
}>(function Submenu({ popupState, item: x }, ref) {
  const theme = useTheme();
  const { pathname } = useRouter();
  const inPath = getHrefs(x).find(y => pathname.startsWith(y));

  if (x.type === 'link') {
    if (popupState) {
      return <MenuItem ref={ref} component={NextLinkComposed} href={x.href} onClick={() => popupState.close()}><a>{x.text}</a></MenuItem>
    } else {
      return <Button ref={ref} sx={[{
        ...theme.mixins.toolbar,
        color: theme.palette.grey[500],
        fontWeight: 'bold',
        borderRadius: 0,
        transitionProperty: 'background-color, box-shadow',
        "&:hover": {
          color: theme.palette.common.white,
          borderBottom: '3px solid white',
        },
      }, inPath ? {
        '&:not(:hover)': {
          color: theme.palette.common.white,
          borderBottom: '3px solid white',
        },
      } : {}]} component={NextLinkComposed} href={x.href}>{x.text}</Button>;
    }
  }

  if (!popupState) {
    return <PopupState variant="popover" popupId={`menu-${x.text.replace(' ', '')}`}>
      {(popupState) => <React.Fragment>
        <Button ref={ref}
          {...bindTrigger(popupState)}
          color="inherit"
          endIcon={<KeyboardArrowDownIcon />}
          sx={[{
            ...theme.mixins.toolbar,
            color: theme.palette.grey[500],
            fontWeight: 'bold',
            borderRadius: 0,
            transitionProperty: 'background-color, box-shadow',
            "&:hover": {
              color: theme.palette.common.white,
              borderBottom: '3px solid white',
            },
          }, inPath ? {
            '&:not(:hover)': {
              color: theme.palette.common.white,
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
              backgroundColor: alpha(theme.palette.common.white, .9),
              borderRadius: 0,
            },
            '& .MuiListItem-button': {
              fontVariant: 'small-caps',
              display: 'flex',
            },
            '& .MuiListItem-button:hover': {
              color: theme.palette.primary.main,
            },
          }}
        >
          {x.children.map(y => <Submenu key={y.text} popupState={popupState} item={y} />)}
        </Menu>
      </React.Fragment>}
    </PopupState>;
  } else {
    return (
      <NestedMenuItem ref={ref} label={x.text} parentMenuOpen={!!popupState.isOpen}>
        {x.children.map(y => <Submenu key={y.text} popupState={popupState} item={y} />)}
      </NestedMenuItem>
    );
  }
});

export const DesktopMenu = () => {
  const menu = useMenu();
  return <React.Fragment>
    {menu.map(x => <Submenu key={x.text} item={x} />)}
  </React.Fragment>;
}
