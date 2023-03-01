import classNames from 'classnames';
import { Dropdown, DropdownItem } from 'components/Dropdown';
import { OlympLogoVertical } from 'components/Icons';
import { SocialButtons } from './SocialButtons';
import { getHrefs, MenuStructItem, useMemberMenu, useTopMenu } from 'lib/data/use-menu';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';
import { ChevronDown, Menu as MenuIcon, User as Account } from 'react-feather';
import { MobileLogo } from './MobileLogo';
import { useAuth } from 'lib/data/use-auth';

export const Header = ({ isOpen, setIsOpen, showTopMenu }: {
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu?: boolean;
}) => {
  const topMenu = useTopMenu();
  const auth = useAuth();

  return (
    <div className="static w-full text-white bg-stone-800">
      <div className="container relative max-w-5xl mx-auto">
        {showTopMenu && (
          <div className="relative hidden lg:flex items-stretch justify-between min-h-[48px] md:min-h-[64px]">
            <DesktopLogo />
            {topMenu.map(x => <DesktopMenuItem key={x.title} item={x} />)}
            {auth.user ? (
              <AuthButton />
            ) : (
              <Link href="/login" passHref>
                <a className="flex items-center gap-2 uppercase font-bold text-sm">
                  <Account className="h-4 w-4" />
                  Pro členy
                </a>
              </Link>
            )}
            <SocialButtons variant="medium" />
          </div>
        )}

        <div className="flex lg:hidden items-stretch justify-between min-h-[48px] md:min-h-[64px] p-2">
          <button className="flex items-center button-icon p-0 m-1" onClick={() => setIsOpen(!isOpen)}>
            <MenuIcon className="w-5 h-5" />
          </button>
          <MobileLogo />
          <Link href={auth.user ? "/profile" : "/login"} passHref>
            <a className="flex items-center button-icon p-0 m-1" >
              <Account className="w-5 h-5" />
            </a>
          </Link>
        </div>
      </div>
    </div>
  );
};

const AuthButton = () => {
  const auth = useAuth();
  const memberMenu = useMemberMenu();

  const button = (
    <div className="flex normal-case button button-text gap-2 items-center">
      <Account className="h-4 w-4" />
      <div className="flex flex-col justify-center items-start" style={{ lineHeight: 1.3 }}>
        <span className="text-xs uppercase tracking-wider">Přihlášen</span>
        <span className="text-sm font-normal">{auth.user?.uJmeno} {auth.user?.uPrijmeni}</span>
      </div>
    </div>
  );
  return <Dropdown
    buttonClassName="min-h-[48px] md:min-h-[64px]"
    align="end"
    button={button}
    options={(memberMenu as DropdownItem[]).concat([
      { title: 'Odhlásit se', onClick: auth.signOut },
    ])}
  />;
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

const DesktopMenuItem = ({ item: x }: { item: MenuStructItem }) => {
  const { pathname } = useRouter();
  const inPath = !!getHrefs(x).find(y => y === "/" ? pathname === "/" : pathname.startsWith(y));

  const cx = classNames(
    "flex gap-1 rounded-none transition-colors",
    "uppercase text-sm font-bold justify-center items-center",
    "hover:text-white hover:border-b-[3px] border-white",
    inPath ? 'text-white border-b-[3px] tracking-wide' : 'text-stone-300',
  );
  if (x.type === 'link') {
    return <Link href={x.href} passHref>
      <a className={cx}>{x.title}</a>
    </Link>;
  }
  return <Dropdown
    align="center"
    buttonClassName={"h-full " + cx}
    button={<>{x.title} <ChevronDown className="w-4 h-4" /></>}
    options={x.children}
  />
};
