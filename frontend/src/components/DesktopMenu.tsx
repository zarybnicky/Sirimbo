import * as React from 'react';
import clsx from 'clsx';
import { NavLink, useLocation } from 'react-router-dom';
import { alpha, Button, Menu, MenuItem, makeStyles } from '@material-ui/core';
import { PopupState as PopupStateType } from 'material-ui-popup-state/core';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { NestedMenuItem } from "./NestedMenuItem";
import { useMenu, MenuStructItem, getHrefs } from '../data/use-menu';

import KeyboardArrowDownIcon from '@material-ui/icons/KeyboardArrowDown';

const useStyles = makeStyles((theme) => ({
  menuButton: {
    ...theme.mixins.toolbar,
    color: theme.palette.grey[500],
    fontWeight: 'bold',
    borderRadius: 0,
    transitionProperty: 'background-color, box-shadow',
    "&:hover": {
      color: theme.palette.common.white,
      borderBottom: '3px solid white',
    },
  },
  activeMenuButton: {
    '&:not(:hover)': {
      color: theme.palette.common.white,
      borderBottom: '3px solid white',
    },
  },
  submenu: {
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
  },
}));

const Submenu = React.forwardRef<any, {
  popupState?: PopupStateType;
  item: MenuStructItem;
}>(({ popupState, item: x }, ref) => {
  const classes = useStyles();
  const { pathname } = useLocation();
  const inPath = getHrefs(x).find(y => pathname.startsWith(y));
  const cls = clsx(classes.menuButton, inPath ? classes.activeMenuButton : null);

  if (x.type === 'link') {
    if (popupState) {
      return <MenuItem ref={ref} button component={NavLink} to={x.href} onClick={() => popupState.close()}>{x.text}</MenuItem>
    } else {
      return <Button ref={ref} className={cls} component={NavLink} to={x.href}>{x.text}</Button>;
    }
  }

  if (!popupState) {
    return <PopupState variant="popover" popupId={`menu-${x.text.replace(' ', '')}`}>
      {(popupState) => <React.Fragment>
        <Button ref={ref}
          {...bindTrigger(popupState)}
          className={cls}
          color="inherit"
          endIcon={<KeyboardArrowDownIcon />}
        >{x.text}</Button>
        <Menu
          {...bindMenu(popupState)}
          getContentAnchorEl={null}
          anchorOrigin={{ vertical: 'bottom', horizontal: 'left' }}
          transformOrigin={{ vertical: 'top', horizontal: 'left' }}
          className={classes.submenu}
        >
          {x.children.map(y => <Submenu key={y.text} popupState={popupState} item={y} />)}
        </Menu>
      </React.Fragment>}
    </PopupState>;
  } else {
    return (
      <NestedMenuItem
        ref={ref} label={x.text}
        parentMenuOpen={!!popupState.isOpen}
        ContainerProps={{ className: classes.submenu }}
      >
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
