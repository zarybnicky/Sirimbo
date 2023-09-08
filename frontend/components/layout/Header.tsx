import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuLabel,
  DropdownMenuLink,
  DropdownMenuTrigger,
} from '@app/ui/dropdown';
import { SocialIcons, MobileLogo, DesktopLogo } from '@app/tenant/current/ui';
import { buttonCls } from '@app/ui/style';
import { useAuth } from '@app/ui/use-auth';
import classNames from 'classnames';
import { getHrefs, MenuStructItem, memberMenu, topMenu } from '@/lib/use-menu';
import {ChevronDown, Menu as MenuIcon, User as Account} from 'lucide-react';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';

type Props = {
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu?: boolean;
};

export const Header = ({ isOpen, setIsOpen, showTopMenu }: Props) => {
  const auth = useAuth();

  return (
    <div className="sticky z-[1100] top-0 inset-x-0 text-white bg-stone-800 shadow-lg">
      <div className="lg:container lg:max-w-6xl relative">
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

            <SocialIcons />
          </div>
        )}

        <div className="flex lg:hidden items-stretch justify-between min-h-[48px] md:min-h-[64px] p-2">
          <button
            className={buttonCls({ className: 'm-1', size: 'lg', variant: 'none' })}
            onClick={() => setIsOpen(!isOpen)}
          >
            <MenuIcon />
          </button>

          <div className="grow flex items-center">
            <MobileLogo />
          </div>

          <Link
            className={buttonCls({ className: 'm-1', size: 'lg', variant: 'none' })}
            href={auth.user ? '/profil' : '/login'}
          >
            <Account />
          </Link>
        </div>
      </div>
    </div>
  );
};

const AuthButton = () => {
  const auth = useAuth();

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
              {auth.user?.uEmail}
            </span>
          </div>
        </button>
      </DropdownMenuTrigger>

      <DropdownMenuContent align="end">
        {memberMenu.map(x => x.type === 'link' ? x : {
          ...x,
          children: x.children.filter(item => (
            !(item.requireTrainer && !auth.perms.isTrainerOrAdmin) &&
            !(item.requireAdmin && !auth.perms.isAdmin))
          )
        }).map((item) =>
          item.type === 'link' ? (
            <DropdownMenuLink key={JSON.stringify(item.href)} href={item.href}>
              {item.title}
            </DropdownMenuLink>
          ) : (
            <React.Fragment key={item.title}>
              <DropdownMenuLabel>{item.title}</DropdownMenuLabel>
              {item.children.map((item) => (
                <DropdownMenuLink key={JSON.stringify(item.href)} href={item.href}>
                  {item.title}
                </DropdownMenuLink>
              ))}
            </React.Fragment>
          ),
        )}
        <DropdownMenuButton onClick={() => auth.signOut()}>
          Odhlásit se
        </DropdownMenuButton>
      </DropdownMenuContent>
    </DropdownMenu>
  );
};

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
      <DropdownMenuContent align="center">
        {x.children.map((item) => (
          <DropdownMenuLink key={JSON.stringify(item.href)} href={item.href}>
            {item.title}
          </DropdownMenuLink>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
};
