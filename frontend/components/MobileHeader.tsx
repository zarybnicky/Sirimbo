import * as React from 'react';
import { AppBar, SwipeableDrawer, Toolbar } from '@mui/material';
import { MenuStructItem, useTopMenu, useSideMenu, getHrefs } from 'lib/data/use-menu';
import { useAuth } from 'lib/data/use-auth';
import { useRouter } from 'next/router';
import { OlympLogoOneline } from 'components/Icons';
import AccountCircle from '@mui/icons-material/AccountCircle';
import MenuIcon from '@mui/icons-material/Menu';
import Link from 'next/link';
import classNames from 'classnames';

export const MobileSubmenu = ({ item: x, onClick }: {
  item: MenuStructItem;
  onClick?: React.MouseEventHandler;
}) => {
  const { pathname } = useRouter();
  const inPath = !!getHrefs(x).find(y => pathname.startsWith(y));

  if (x.type === 'link') {
    return <Link href={x.href} passHref>
      <a onClick={onClick} className={classNames(
        "rounded-2xl tracking-wide text-xm px-3 py-1.5",
        "flex items-center grow mx-2 hover:bg-stone-500 hover:text-white",
        inPath ? 'bg-stone-700' : '',
      )}>
        <div className={`font-light grow ${inPath ? 'text-white' : ''}`}>{x.text}</div>
      </a>
    </Link>
  }

  return <>
    <div key={x.text} className="ml-3 mt-4 mb-2">
      <div className="font-bold text-xs uppercase grow my-1">{x.text}</div>
    </div>
    <div className="list-none grid gap-0.5">
      {x.children.map(y => <MobileSubmenu key={y.text} item={y} onClick={onClick} />)}
    </div>
  </>;
};

export const MobileHeader = ({ }) => {
  const auth = useAuth();
  const topMenu = useTopMenu();
  const sideMenu = useSideMenu();
  const router = useRouter();
  const [open, setOpen] = React.useState(false);

  return <div className="block md:hidden">
    <AppBar position="static" color="primary">
      <Toolbar>
        <div className="grow flex items-center">
          <OlympLogoOneline viewBox="0 0 381.82217 111.78744" width="170" height="50" style={{
            filter: 'drop-shadow(0px 6px 6px rgba(0, 0, 0, 0.2))',
            color: 'white',
            fill: 'white !important',
          }} />
        </div>
        <Link href="/profile" passHref>
          <a className="button button-icon p-0 m-1" >
            <AccountCircle />
          </a>
        </Link>
        <button className="button button-icon p-0 m-1" onClick={() => setOpen(!open)}>
          <MenuIcon />
        </button>
      </Toolbar>
    </AppBar>
    <SwipeableDrawer
      variant="temporary"
      anchor='left'
      open={open}
      onOpen={() => setOpen(true)}
      onClose={() => setOpen(false)}
    >
      <div className="list-none w-[85vw] max-w-[450px]">
        {auth.user ? <>
          <MobileSubmenu item={{ type: 'link', text: 'Profil', href: '/profile' }} onClick={() => setOpen(false)} />
          <MobileSubmenu item={{ type: 'link', text: 'Odhlásit se', href: '/' }} onClick={() => {
            setOpen(false);
            auth.signOut();
            router.push('/');
          }} />
        </> : (
          <MobileSubmenu item={{ type: 'link', text: 'Přihlásit se', href: '/login' }} onClick={() => setOpen(false)} />
        )}
        {sideMenu.map(x => <MobileSubmenu key={x.text} item={x} onClick={() => setOpen(false)} />)}
        <MobileSubmenu item={{ type: 'link', text: 'Domů', href: '/home' }} onClick={() => setOpen(false)} />
        {topMenu.map(x => <MobileSubmenu key={x.text} item={x} onClick={() => setOpen(false)} />)}
      </div>
    </SwipeableDrawer>
  </div>;
}
