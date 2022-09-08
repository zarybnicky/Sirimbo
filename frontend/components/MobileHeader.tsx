import * as React from 'react';
import {
  AppBar, Collapse, Divider, IconButton, SwipeableDrawer, Toolbar, List,
  ListItem, ListItemText, makeStyles
} from '@material-ui/core';
import { NavLink, useHistory, useLocation } from 'react-router-dom';
import { MenuStructItem, useMenu, getHrefs } from '../data/use-menu';

import OlympLogo from '../../static/images/olymp-logo-oneline.svg';
import AccountCircle from '@material-ui/icons/AccountCircle';
import MenuIcon from '@material-ui/icons/Menu';
import ExpandLess from '@material-ui/icons/ExpandLess';
import ExpandMore from '@material-ui/icons/ExpandMore';
import { useAuth } from '../data/use-auth';

const useStyles = makeStyles((theme) => ({
  svg: {
    filter: 'drop-shadow(0px 6px 6px rgba(0, 0, 0, 0.2))',
    '& *': {
      color: 'white',
      fill: 'white !important',
    },
  },
  header: {
    flexGrow: 1,
    display: 'flex',
    alignItems: 'center',
  },
  list: {
    maxWidth: '450px',
    width: '85vw',
  }
}));

const Submenu = ({ level = 0, item: x, onClick }: {
  level?: number;
  item: MenuStructItem;
  onClick: React.MouseEventHandler;
}) => {
  const { pathname } = useLocation();
  const inPath = !!getHrefs(x).find(y => pathname.startsWith(y));
  const [open, setOpen] = React.useState(inPath);

  if (x.type === 'link') {
    return <ListItem button component={NavLink} to={x.href} onClick={onClick}>
      <ListItemText primary={x.text} style={{ marginLeft: `${level}rem` }} />
    </ListItem>
  }

  return <React.Fragment>
    <ListItem key={x.text} button onClick={() => setOpen(!open)}>
      <ListItemText primary={x.text} style={{ marginLeft: `${level}rem` }} />
      {open ? <ExpandLess /> : <ExpandMore />}
    </ListItem>
    <Collapse in={open} timeout="auto" unmountOnExit>
      <List disablePadding>
        {x.children.map(y => <Submenu level={level + 1} key={y.text} item={y} onClick={onClick} />)}
      </List>
    </Collapse>
    <Divider />
  </React.Fragment>;
};

export const MobileHeader = ({ }) => {
  const classes = useStyles();
  const auth = useAuth();
  const menu = useMenu();
  const history = useHistory();
  const [open, setOpen] = React.useState(false);

  return <div>
    <AppBar position="static" color="primary">
      <Toolbar>
        <div className={classes.header}>
          <OlympLogo viewBox="0 0 381.82217 111.78744" width="170" height="50" className={classes.svg} />
        </div>
        <IconButton color="inherit" component={NavLink} to="/profile"
        ><AccountCircle /></IconButton>
        <IconButton color="inherit" onClick={() => setOpen(!open)}><MenuIcon /></IconButton>
      </Toolbar>
    </AppBar>
    <SwipeableDrawer
      variant="temporary"
      anchor='left'
      open={open}
      onOpen={() => setOpen(true)}
      onClose={() => setOpen(false)}
    >
      <List className={classes.list}>
        <Submenu item={{ type: 'link', text: 'Domů', href: '/home' }} onClick={() => setOpen(false)} />
        {menu.map(x => <Submenu key={x.text} item={x} onClick={() => setOpen(false)} />)}
        <Divider />
        {auth.user ? (<>
          <Submenu item={{ type: 'link', text: 'Profil', href: '/profile' }} onClick={() => setOpen(false)} />
          <Submenu item={{ type: 'link', text: 'Odhlásit se', href: '/' }} onClick={() => {
            setOpen(false);
            auth.signOut();
            history.push('/');
          }} />
        </>) : (
          <Submenu item={{ type: 'link', text: 'Přihlásit se', href: '/login' }} onClick={() => setOpen(false)} />
        )}
      </List>
    </SwipeableDrawer>
  </div>;
}
