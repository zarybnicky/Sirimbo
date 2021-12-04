import * as React from 'react';
import {
  AppBar, Collapse, IconButton, SwipeableDrawer, Toolbar, Typography, List,
  ListItem, ListItemText, Divider,
} from '@material-ui/core';
import { NavLink } from 'react-router-dom';
import { makeStyles } from '@material-ui/core/styles';
import ChevronLeft from '@material-ui/icons/ChevronLeft';
import MenuIcon from '@material-ui/icons/Menu';
import ExpandLess from '@material-ui/icons/ExpandLess';
import ExpandMore from '@material-ui/icons/ExpandMore';

import { MenuType, SubmenuType } from '../use-menu';

const useStyles = makeStyles((theme) => ({
  drawerButton: {
    marginRight: theme.spacing(2),
  },
}));

const Submenu = ({ item: x }: { item: SubmenuType }) => {
  const [open, setOpen] = React.useState(false);
  return <React.Fragment>
    <ListItem key={x.text} button onClick={() => setOpen(!open)}>
      <ListItemText primary={x.text} />
      {open ? <ExpandLess /> : <ExpandMore />}
    </ListItem>
    <Collapse in={open} timeout="auto" unmountOnExit>
      <List disablePadding>
        {x.children.map(x => (
          <ListItem key={x.text} button component={NavLink} to={x.href}>
            <ListItemText primary={x.text} />
          </ListItem>
        ))}
      </List>
    </Collapse>
  </React.Fragment>;
};

export const MobileHeader = ({ menu }: { menu: MenuType }) => {
  const classes = useStyles();
  const [open, setOpen] = React.useState(false);

  return <React.Fragment>
    <AppBar position="static" color="primary">
      <Toolbar>
        <IconButton edge="start" className={classes.drawerButton} color="inherit" aria-label="menu">
          <MenuIcon />
        </IconButton>
        <Typography variant="h6" color="inherit">TK Olymp</Typography>
      </Toolbar>
    </AppBar>
    <SwipeableDrawer
      variant="temporary"
      anchor='left'
      open={open}
      onOpen={() => setOpen(true)}
      onClose={() => setOpen(false)}
    >
      <div>
        <NavLink to="/" style={{ textDecoration: 'none', color: 'white' }}>
          <img src="/images/Music-PNG-Photos.png" alt="logo" style={{ width: '120px', height: '70px' }} />
        </NavLink>
        <IconButton onClick={() => setOpen(false)}><ChevronLeft /></IconButton>
      </div>
      <List>
        {menu.map(x => x.type === 'link'
          ? (
            <ListItem key={x.text} button component={NavLink} to={x.href}>
              <ListItemText primary={x.text} />
            </ListItem>
          ) : <Submenu item={x} />
        )}
        <Divider />
      </List>
    </SwipeableDrawer>
  </React.Fragment>;
}
