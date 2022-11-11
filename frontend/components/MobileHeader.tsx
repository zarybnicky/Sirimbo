import * as React from 'react';
import {
  AppBar, Divider, IconButton, SwipeableDrawer, Toolbar, List,
  ListItem, ListItemText, Box
} from '@mui/material';
import { MenuStructItem, useTopMenu, useSideMenu, getHrefs } from 'lib/data/use-menu';
import { useAuth } from 'lib/data/use-auth';
import { useRouter } from 'next/router';
import { NextLinkComposed } from './Link';
import { OlympLogoOneline } from 'components/Icons';
import AccountCircle from '@mui/icons-material/AccountCircle';
import MenuIcon from '@mui/icons-material/Menu';

export const MobileSubmenu = ({ item: x, onClick }: {
  item: MenuStructItem;
  onClick?: React.MouseEventHandler;
}) => {
  const { pathname } = useRouter();
  const inPath = !!getHrefs(x).find(y => pathname.startsWith(y));

  if (x.type === 'link') {
    return <ListItem
      button component={NextLinkComposed} href={x.href} onClick={onClick}
      disablePadding sx={{
        width: 'calc(100% - 1rem)',
        margin: '.2rem .5rem .2rem .5rem',
        paddingLeft: '.5rem',
        borderRadius: 3,
        backgroundColor: inPath ? '#777' : undefined,
      }}
    >
      <ListItemText primary={x.text} sx={{
        fontWeight: 'light',
        color: inPath ? 'white' : undefined,
      }} />
    </ListItem >
  }

  return <>
    <ListItem key={x.text} disablePadding sx={{
      padding: '1rem 1rem 0.2rem',
    }}>
      <ListItemText primary={x.text} primaryTypographyProps={{
        sx: {
          fontWeight: 'bold',
          fontSize: 13,
        }
      }} />
    </ListItem>
    <List disablePadding>
      {x.children.map(y => <MobileSubmenu key={y.text} item={y} onClick={onClick} />)}
    </List>
  </>;
};

export const MobileHeader = ({ }) => {
  const auth = useAuth();
  const topMenu = useTopMenu();
  const sideMenu = useSideMenu();
  const router = useRouter();
  const [open, setOpen] = React.useState(false);

  return <div>
    <AppBar position="static" color="primary">
      <Toolbar>
        <Box sx={{
          flexGrow: 1,
          display: 'flex',
          alignItems: 'center',
        }}>
          <OlympLogoOneline viewBox="0 0 381.82217 111.78744" width="170" height="50" style={{
            filter: 'drop-shadow(0px 6px 6px rgba(0, 0, 0, 0.2))',
            color: 'white',
            fill: 'white !important',
          }} />
        </Box>
        <IconButton color="inherit" LinkComponent={NextLinkComposed} href="/profile">
          <AccountCircle />
        </IconButton>
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
      <List sx={{
        maxWidth: '450px',
        width: '85vw',
      }}>
        <MobileSubmenu item={{ type: 'link', text: 'Domů', href: '/home' }} onClick={() => setOpen(false)} />
        {sideMenu.length > 0 ? (
          <MobileSubmenu item={{ type: 'menu', text: 'Pro veřejnost', children: topMenu }} onClick={() => setOpen(false)} />
        ) : (
          topMenu.map(x => <MobileSubmenu key={x.text} item={x} onClick={() => setOpen(false)} />)
        )}
        <Divider />
        {sideMenu.map(x => <MobileSubmenu key={x.text} item={x} onClick={() => setOpen(false)} />)}
        {auth.user ? (<>
          <MobileSubmenu item={{ type: 'link', text: 'Profil', href: '/profile' }} onClick={() => setOpen(false)} />
          <MobileSubmenu item={{ type: 'link', text: 'Odhlásit se', href: '/' }} onClick={() => {
            setOpen(false);
            auth.signOut();
            router.push('/');
          }} />
        </>) : (
          <MobileSubmenu item={{ type: 'link', text: 'Přihlásit se', href: '/login' }} onClick={() => setOpen(false)} />
        )}
      </List>
    </SwipeableDrawer>
  </div>;
}
