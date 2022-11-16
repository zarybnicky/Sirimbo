import * as React from 'react';
import Link from 'next/link';
import { Footer } from './Footer';
import { SocialButtons } from './SocialButtons';
import { AuthButton } from './AuthButton';
import { OlympLogoVertical, OlympLogoOneline } from 'components/Icons';
import { MenuStructItem, getHrefs, useTopMenu, useSideMenu } from 'lib/data/use-menu';
import { useRouter } from 'next/router';
import { ChevronDown, User as AccountCircle, Menu as MenuIcon } from 'react-feather';
import classNames from 'classnames';
import { Dropdown } from './Dropdown';
import { useAuth } from 'lib/data/use-auth';

export const Layout: React.FC = ({ children }) => {
  const topMenu = useTopMenu();
  const sideMenu = useSideMenu();
  const [isOpen, setIsOpen] = React.useState(false);
  const router = useRouter();
  const auth = useAuth();

  return <div className="h-screen w-full overflow-hidden">
    <div className="static w-full text-white bg-stone-800">
      <div className="container relative max-w-5xl mx-auto">
        <div className="hidden md:flex items-stretch justify-between min-h-[48px] md:min-h-[64px]">
          <DesktopLogo />
          {topMenu.map(x => <DesktopMenuItem key={x.title} item={x} />)}
          <SocialButtons className="flex items-center" variant="medium" />
          <AuthButton />
        </div>

        <div className="flex md:hidden items-stretch justify-between min-h-[48px] md:min-h-[64px] p-2">
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
          <button className="button button-icon p-0 m-1" onClick={() => setIsOpen(!isOpen)}>
            <MenuIcon />
          </button>
        </div>
      </div>
    </div>

    <div className="relative h-full flex">
      <nav className={classNames(
        isOpen ? 'absolute inset-y-0 left-0 translate-x-0 shadow-lg' : 'absolute -translate-x-full',
        "w-3/4 sm:w-1/2 md:w-1/3 lg:w-56 xl:w-64 2xl:w-72 3xl:w-80",
        "z-30 h-full max-h-screen min-h-screen flex-none transform overflow-y-auto border-r border-gray-150 bg-white pb-10 transition duration-200 ease-in-out dark:border-gray-800 dark:bg-gray-900 sm:pb-0 md:w-1/3 lg:relative lg:z-auto lg:translate-x-0 lg:bg-gray-50 lg:dark:bg-gray-900",
      )}>
        <div className="grid gap-1 pt-3">
          {sideMenu.map(x => <MobileSubmenu key={x.title} item={x} onClick={() => setIsOpen(false)} />)}
          <MobileSubmenu item={{ type: 'link', title: 'Domů', href: '/home' }} onClick={() => setIsOpen(false)} />
          {topMenu.map(x => <MobileSubmenu key={x.title} item={x} onClick={() => setIsOpen(false)} />)}

          {auth.user ? <>
            <MobileSubmenu item={{ type: 'link', title: 'Profil', href: '/profile' }} onClick={() => setIsOpen(false)} />
            <MobileSubmenu item={{ type: 'link', title: 'Odhlásit se', href: '/' }} onClick={() => {
              setIsOpen(false);
              auth.signOut();
              router.push('/');
            }} />
          </> : (
            <MobileSubmenu item={{ type: 'link', title: 'Přihlásit se', href: '/login' }} onClick={() => setIsOpen(false)} />
          )}
        </div>
      </nav>

      <div className="flex flex-1 flex-col overflow-y-auto">
        {children}
        <Footer />
      </div>
    </div>
    <div
      className={`fixed inset-0 z-20 bg-black bg-opacity-10 transition duration-200 ease-in-out dark:bg-opacity-50 ${isOpen
        ? 'pointer-events-auto opacity-100'
        : 'pointer-events-none opacity-0'
        }`}
      onClick={() => setIsOpen(false)}
    />
  </div>;
};

const DesktopLogo = () => (
  <div className="relative overflow-visible min-w-[104px]">
    <div className="w-[104px] h-[130px] text-white bg-red-500 z-50 shadow-red-900/40 shadow-lg absolute top-0 left-0 right-0">
      <Link passHref href="/">
        <a className="block p-0 m-0 h-full w-full relative">
          <OlympLogoVertical style={{
            filter: 'drop-shadow(0px 6px 6px rgba(0, 0, 0, 0.2))',
            position: 'absolute',
            left: 0,
            bottom: 0,
            width: '104px',
            height: '104px',
            color: 'white',
            fill: 'white !important',
          }} />
        </a>
      </Link>
    </div>
  </div>
);

export const MobileSubmenu = ({ item: x, onClick }: {
  item: MenuStructItem;
  onClick?: React.MouseEventHandler;
}) => {
  const { pathname } = useRouter();
  const inPath = !!getHrefs(x).find(y => pathname.startsWith(y));

  if (x.type === 'link') {
    return <Link href={x.href} passHref>
      <a onClick={onClick} className={classNames(
        "rounded-2xl px-3 py-1.5",
        "flex items-center grow mx-2 hover:bg-stone-500 hover:text-white",
        "font-light tracking-wide text-sm",
        inPath ? 'bg-stone-700 text-white' : '',
      )}>
        {x.title}
      </a>
    </Link>
  }

  return <>
    <div key={x.title} className="ml-3 mt-2">
      <div className="font-bold text-xs uppercase grow my-1">{x.title}</div>
    </div>
    <div className="list-none grid gap-0.5 ml-2">
      {x.children.map(y => <MobileSubmenu key={y.title} item={y} onClick={onClick} />)}
    </div>
  </>;
};

const DesktopMenuItem = ({ item: x }: { item: MenuStructItem }) => {
  const { pathname } = useRouter();
  const inPath = getHrefs(x).find(y => pathname.startsWith(y));

  const cx = classNames(
    "flex gap-1 rounded-none transition-colors",
    "uppercase text-xs tracking-wide items-center text-grey-500",
    "hover:text-white hover:border-b-3 hover:border-white",
    inPath && 'text-white border-b-3 border-white'
  );
  if (x.type === 'link') {
    return <Link href={x.href} passHref>
      <a className={cx}>{x.title}</a>
    </Link>;
  }
  const button = (
    <button className={cx}>
      {x.title}
      <ChevronDown className="w-4.5 h-4.5" />
    </button>
  );
  return <Dropdown align="center" button={button} options={x.children} />
};
