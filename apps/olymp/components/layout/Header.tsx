import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuLink, DropdownMenuTrigger } from '@app/ui/dropdown';
import { OlympLogoOneline, OlympLogoVertical } from '@app/ui/Icons';
import { useAuth } from '@app/ui/use-auth';
import classNames from 'classnames';
import { getHrefs, MenuStructItem, useMemberMenu, useTopMenu } from 'lib/use-menu';
import { ChevronDown, Facebook, Instagram, Menu as MenuIcon, User as Account, Youtube } from 'lucide-react';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';

type Props = {
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu?: boolean;
};

export const Header = ({ isOpen, setIsOpen, showTopMenu }: Props) => {
  const topMenu = useTopMenu();
  const auth = useAuth();

  return (
    <div className="static w-full text-white bg-primary shadow-lg border-b border-red-700">
      <div className="container relative max-w-5xl mx-auto">
        {showTopMenu && (
          <div className="relative hidden lg:flex items-stretch justify-between min-h-[48px] md:min-h-[64px]">
            <DesktopLogo />
            {topMenu.map((x) => (
              <DesktopMenuItem key={x.title} item={x} />
            ))}
            {auth.user ? (
              <AuthButton />
            ) : (
              <Link
                href="/login"
                className="flex items-center gap-2 uppercase font-bold text-sm"
              >
                <Account className="h-4 w-4" />
                Pro členy
              </Link>
            )}
            <div className="flex gap-1 items-center">
              <a
                target="_blank"
                rel="noreferrer"
                href="https://www.facebook.com/tkolymp"
                className="p-1"
              >
                <Facebook className="text-stone-200" />
              </a>
              <a
                target="_blank"
                rel="noreferrer"
                href="https://www.instagram.com/tanecni_klub_olymp"
                className="p-1"
              >
                <Instagram className="text-stone-100" />
              </a>
              <a
                target="_blank"
                rel="noreferrer"
                href="https://www.youtube.com/user/TheMamcro"
                className="p-1"
              >
                <Youtube className="text-white" />
              </a>
            </div>
          </div>
        )}

        <div className="flex lg:hidden items-stretch justify-between min-h-[48px] md:min-h-[64px] p-2">
          <button
            className="flex items-center button-icon p-0 m-1"
            onClick={() => setIsOpen(!isOpen)}
          >
            <MenuIcon className="w-5 h-5" />
          </button>

          <div className="grow flex items-center">
            <OlympLogoOneline
              viewBox="0 0 381.82217 111.78744"
              width="170"
              height="50"
              style={{
                filter: 'drop-shadow(0px 6px 6px rgba(0, 0, 0, 0.2))',
                color: 'white',
                fill: 'white !important',
              }}
            />
          </div>

          <Link
            href={auth.user ? '/profile' : '/login'}
            className="flex items-center button-icon p-0 m-1"
          >
            <Account className="w-5 h-5" />
          </Link>
        </div>
      </div>
    </div>
  );
};

const AuthButton = () => {
  const auth = useAuth();
  const memberMenu = useMemberMenu();

  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <button className="min-h-[48px] md:min-h-[64px] flex gap-2 items-center drop-shadow">
          <Account className="h-4 w-4" />
          <div
            className="flex flex-col justify-center items-start"
            style={{ lineHeight: 1.3 }}
          >
            <span className="text-xs uppercase tracking-wider">Přihlášen</span>
            <span className="text-sm font-normal">
              {auth.user?.uJmeno} {auth.user?.uPrijmeni}
            </span>
          </div>
        </button>
      </DropdownMenuTrigger>

      <DropdownMenuContent align="end">
        {memberMenu.map(item => (
          <DropdownMenuLink key={JSON.stringify(item.href)} href={item.href}>
            {item.title}
          </DropdownMenuLink>
        ))}
        <DropdownMenuButton onClick={() => auth.signOut()}>
          Odhlásit se
        </DropdownMenuButton>
      </DropdownMenuContent>
    </DropdownMenu>
  );
};

const DesktopLogo = () => (
  <div className="relative overflow-visible min-w-[104px]">
    <div className="w-[104px] h-[130px] text-white bg-stone-800 z-30 shadow-stone-900/40 shadow-lg absolute top-0 inset-x-0">
      <Link href="/" className="block p-0 m-0 h-full w-full relative">
        <OlympLogoVertical
          style={{
            filter: 'drop-shadow(0px 6px 6px rgba(0, 0, 0, 0.2))',
            position: 'absolute',
            left: 0,
            bottom: 0,
            width: '104px',
            height: '104px',
            color: 'white',
            fill: 'white !important',
          }}
        />
      </Link>
    </div>
  </div>
);

const DesktopMenuItem = ({ item: x }: { item: MenuStructItem }) => {
  const { pathname } = useRouter();
  const inPath = !!getHrefs(x).find((y) =>
    y === '/' ? pathname === '/' : pathname.startsWith(y),
  );

  const cx = classNames(
    'flex gap-1 rounded-none transition-colors',
    'uppercase text-sm font-bold justify-center items-center',
    'hover:text-white hover:border-b-[3px] border-white data-[state=open]:border-b-[3px]',
    inPath
      ? 'text-white drop-shadow-xl border-b-[3px] tracking-wide mb-[-1px]'
      : 'text-stone-100 drop-shadow',
  );
  if (x.type === 'link') {
    return (
      <Link href={x.href} className={cx}>
        {x.title}
      </Link>
    );
  }
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <button className={'block ' + cx}>
          {x.title} <ChevronDown className="w-4 h-4" />
        </button>
      </DropdownMenuTrigger>
      <DropdownMenuContent align="center" >
        {x.children.map(item => (
          <DropdownMenuLink key={JSON.stringify(item.href)} href={item.href}>
            {item.title}
          </DropdownMenuLink>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
};
