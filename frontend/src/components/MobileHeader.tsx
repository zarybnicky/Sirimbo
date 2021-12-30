import * as React from 'react';
import {
  AppBar, Collapse, Divider, IconButton, SwipeableDrawer, Toolbar, List,
  ListItem, ListItemText, makeStyles
} from '@material-ui/core';
import { NavLink } from 'react-router-dom';
import { MenuType, SubmenuType } from '../use-menu';
import { useAuth } from '../use-auth';

import OlympLogo from '../../static/images/olymp-logo-oneline.svg';
import AccountCircle from '@material-ui/icons/AccountCircle';
import MenuIcon from '@material-ui/icons/Menu';
import ExpandLess from '@material-ui/icons/ExpandLess';
import ExpandMore from '@material-ui/icons/ExpandMore';

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
    maxWidth: '400px',
    width: '70vw',
  }
}));

const Submenu = ({ item: x, onClick }: { item: SubmenuType; onClick: React.MouseEventHandler }) => {
  const [open, setOpen] = React.useState(true);
  return <React.Fragment>
    <ListItem key={x.text} button onClick={() => setOpen(!open)}>
      <ListItemText primary={x.text} />
      {open ? <ExpandLess /> : <ExpandMore />}
    </ListItem>
    <Collapse in={open} timeout="auto" unmountOnExit>
      <List disablePadding>
        {x.children.map(x => (
          <ListItem key={x.text} button component={NavLink} to={x.href} onClick={onClick}>
            <ListItemText primary={x.text} style={{ marginLeft: '1rem' }} />
          </ListItem>
        ))}
      </List>
    </Collapse>
    <Divider />
  </React.Fragment>;
};

export const MobileHeader = ({ menu }: { menu: MenuType }) => {
  const auth = useAuth();
  const classes = useStyles();
  const [open, setOpen] = React.useState(false);

  return <div>
    <AppBar position="static" color="primary">
      <Toolbar>
        <div className={classes.header}>
          <OlympLogo viewBox="0 0 381.82217 111.78744" width="170" height="50" className={classes.svg} />
        </div>
        <IconButton color="inherit" onClick={() => setOpen(!open)}><AccountCircle /></IconButton>
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
        {menu.map(x => x.type === 'link'
          ? (
            <ListItem key={x.text} button component={NavLink} to={x.href} onClick={() => setOpen(false)}>
              <ListItemText primary={x.text} />
            </ListItem>
          ) : <Submenu key={x.text} item={x} onClick={() => setOpen(false)} />
        )}
      </List>
    </SwipeableDrawer>
  </div>;
}
