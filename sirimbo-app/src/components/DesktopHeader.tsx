import * as React from 'react';
import { NavLink } from 'react-router-dom';
import { AppBar, Box, Button, Container, Icon, IconButton, Menu, MenuItem, Toolbar } from '@material-ui/core';
import { makeStyles } from '@material-ui/core/styles';
import { SocialButtons } from './SocialButtons';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import clsx from 'clsx';
import { AuthContextType } from '../use-auth';
import { MenuType, SubmenuType } from '../use-menu';

import OlympLogoVertical from '../../static/images/olymp-logo-vertical.png';

const useStyles = makeStyles((theme) => ({
  navlinks: {
    justifyContent: 'space-between',
    alignItems: 'center',
    display: "flex",
  },
  logoBox: {
    ...theme.mixins.toolbar,
    position: 'relative',
    overflow: 'visible',
    minWidth: '104px',
  },
  logo: {
    position: 'absolute',
    top: 0,
    left: 0,
    right: 0,
    width: '104px',
    height: '130px',
    verticalAlign: 'bottom',
    backgroundColor: theme.palette.primary.main,
    color: theme.palette.primary.contrastText,
    '& img': {
      position: 'absolute',
      left: 0,
      bottom: '3px',
    }
  },
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
}));

const Submenu = ({ item: x, className, activeClassName }: {
  item: SubmenuType; className: string; activeClassName: string;
}) => (
  <PopupState variant="popover" popupId={`menu-${x.text.replace(' ', '')}`}>
    {(popupState) => <React.Fragment>
      <Button
        className={clsx(className, popupState.isOpen ? activeClassName : null)}
        {...bindTrigger(popupState)}
      >{x.text}</Button>
      <Menu
        {...bindMenu(popupState)}
        getContentAnchorEl={null}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'left' }}
        transformOrigin={{ vertical: 'top', horizontal: 'left' }}
      >
        {x.children.map(x => (
          <MenuItem key={x.text} component={NavLink} to={x.href} onClick={() => popupState.close}>{x.text}</MenuItem>
        ))}
      </Menu>
    </React.Fragment>}
  </PopupState>
);

export const DesktopHeader = ({ menu, auth }: { menu: MenuType; auth: AuthContextType; }) => {
  const classes = useStyles();

  return <AppBar position="static" color="secondary">
    <Toolbar>
      <Container maxWidth="lg" className={classes.navlinks}>
        <div className={classes.logoBox}>
          <Box boxShadow={10} className={classes.logo}>
            <img src={OlympLogoVertical} alt="TK Olymp" />
          </Box>
        </div>
        {menu.map(x => x.type === 'link' ? (
          <Button
            key={x.text}
            className={classes.menuButton} activeClassName={classes.activeMenuButton}
            component={NavLink} to={x.href}
          >{x.text}</Button>
        ) : (
          <Submenu
            key={x.text} item={x}
            className={classes.menuButton} activeClassName={classes.activeMenuButton}
          />
        ))}
        <SocialButtons variant="medium" />
        {auth.user ? (
          <PopupState variant="popover" popupId="demoMenu">
            {(popupState) => <React.Fragment>
              <IconButton {...bindTrigger(popupState)} color="inherit"><Icon>account_circle</Icon></IconButton>
              <Menu
                {...bindMenu(popupState)}
                getContentAnchorEl={null}
                anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
                transformOrigin={{ vertical: 'top', horizontal: 'right' }}
              >
                <MenuItem onClick={popupState.close}>Profile</MenuItem>
                <MenuItem onClick={popupState.close}>My account</MenuItem>
              </Menu>
            </React.Fragment>}
          </PopupState>
        ) : (<div>
          <Button component={NavLink} to="/login" color="inherit" onClick={() => auth.signIn('', '')}>
            <IconButton color="inherit"><Icon>account_circle</Icon></IconButton>
            Přihlásit
          </Button>
        </div>)}
      </Container>
    </Toolbar>
  </AppBar >;
};
