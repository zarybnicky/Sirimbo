import * as React from 'react';
import { Link, NavLink, useRouteMatch } from 'react-router-dom';
import { AppBar, Box, Button, Container, alpha, Menu, MenuItem, Toolbar, makeStyles } from '@material-ui/core';
import { SocialButtons } from './SocialButtons';
import { PopupState as PopupStateType } from 'material-ui-popup-state/core';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import clsx from 'clsx';
import { AuthContextType } from '../use-auth';
import { MenuType, SubmenuType } from '../use-menu';
import { useHistory } from 'react-router';

import OlympLogoVertical from '../../static/images/olymp-logo-vertical.svg';
import AccountCircle from '@material-ui/icons/AccountCircle';

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
    zIndex: 100,
    position: 'absolute',
    top: 0,
    left: 0,
    right: 0,
    width: '104px',
    height: '130px',
    backgroundColor: theme.palette.primary.main,
    color: theme.palette.primary.contrastText,
  },
  logoInner: {
    display: 'block',
    padding: 0,
    margin: 0,
    height: '100%',
    width: '100%',
    position: 'relative',
  },
  svg: {
    filter: 'drop-shadow(0px 6px 6px rgba(0, 0, 0, 0.2))',
    position: 'absolute',
    left: 0,
    bottom: 0,
    width: '104px',
    height: '104px',
    '& *': {
      color: 'white',
      fill: 'white !important',
    },
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

const SubmenuButton = ({ item: x, className, activeClassName, popupState }: {
  item: SubmenuType; className: string; activeClassName: string;
  popupState: PopupStateType;
}) => {
  const match = useRouteMatch(x.hrefRoot);
  return <Button
    className={clsx(className, (popupState.isOpen || match) ? activeClassName : null)}
    {...bindTrigger(popupState)}
  >{x.text}</Button>
};
const Submenu = ({ item: x, className, activeClassName }: {
  item: SubmenuType; className: string; activeClassName: string;
}) => {
  const classes = useStyles();
  return <PopupState variant="popover" popupId={`menu-${x.text.replace(' ', '')}`}>
    {(popupState) => <React.Fragment>
      <SubmenuButton {...{ item: x, className, activeClassName, popupState }} />
      <Menu
        {...bindMenu(popupState)}
        getContentAnchorEl={null}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'left' }}
        transformOrigin={{ vertical: 'top', horizontal: 'left' }}
        className={classes.submenu}
      >
        {x.children.map(x => (
          <MenuItem button
            key={x.text} onClick={() => popupState.close()}
            component={NavLink} to={x.href}
          >
            {x.text}
          </MenuItem>
        ))}
      </Menu>
    </React.Fragment>}
  </PopupState>
};

export const DesktopHeader = ({ menu, auth }: { menu: MenuType; auth: AuthContextType; }) => {
  const classes = useStyles();
  const history = useHistory();

  return <AppBar position="static" color="secondary">
    <Toolbar>
      <Container maxWidth="lg" className={classes.navlinks}>
        <div className={classes.logoBox}>
          <Box boxShadow={10} className={classes.logo}>
            <Link to="/" className={classes.logoInner}>
              <OlympLogoVertical className={classes.svg} />
            </Link>
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
              <Button
                {...bindTrigger(popupState)}
                color="inherit"
                startIcon={<AccountCircle />}
              >
                <div style={{ display: 'flex', justifyContent: 'start', lineHeight: 1.3, flexDirection: 'column' }}>
                  <span style={{ textDecoration: 'underline' }}>Přihlášen</span>
                  <span style={{ textTransform: 'none' }}>{auth.user?.uJmeno} {auth.user?.uPrijmeni}</span>
                </div>
              </Button>
              <Menu
                {...bindMenu(popupState)}
                getContentAnchorEl={null}
                anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
                transformOrigin={{ vertical: 'top', horizontal: 'right' }}
              >
                <MenuItem onClick={() => {
                  popupState.close();
                  auth.signOut();
                  history.push('/');
                }}>Odhlásit se</MenuItem>
              </Menu>
            </React.Fragment>}
          </PopupState>
        ) : (<div>
          <Button
            component={NavLink} to="/login" color="inherit"
            onClick={() => auth.signIn('', '')}
            startIcon={<AccountCircle />}
            style={{ textDecoration: 'underline' }}
          >
            Přihlásit
          </Button>
        </div>)}
      </Container>
    </Toolbar>
  </AppBar >;
};
