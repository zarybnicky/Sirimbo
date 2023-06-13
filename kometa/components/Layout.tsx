import classNames from 'classnames';
import { Dropdown, DropdownItem } from 'components/Dropdown';
import { getHrefs, MenuStructItem, useMemberMenu, useTopMenu } from 'lib/data/use-menu';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';
import { ChevronDown, Menu as MenuIcon, User as Account } from 'lucide-react';
import { useAuth } from 'lib/use-auth';
import { Instagram, Facebook, Youtube } from 'lucide-react';
import { Route, route } from 'nextjs-routes';

export type LayoutProps = {
  list?: React.ReactNode;
  isDetail?: boolean;
  children?: React.ReactNode;
};

export function Layout({
  children,
  list,
  isDetail,
}: LayoutProps) {
  const [isOpen, setIsOpen] = React.useState(false);
  const router = useRouter();

  React.useEffect(() => {
    const scroll = () =>
      document.querySelector('[data-nextjs-scroll-focus-boundary]')?.scrollTo(0, 0);
    router.events.on('routeChangeComplete', scroll);
    return () => router.events.off('routeChangeComplete', scroll);
  }, [router]);

  return (
    <div className="h-screen flex flex-col w-full relative overflow-hidden">
      <Header {...{ isOpen, setIsOpen }} />
      <div className="relative grow flex overflow-hidden bg-neutral-2">
        <Sidebar {...{ isOpen, setIsOpen }} />
        {list && (
          <div
            className={classNames(
              'grow',
              isDetail
                ? 'hidden lg:flex lg:grow-0 flex-col'
                : 'max-h-screen min-h-screen w-full',
            )}
          >
            {list}
          </div>
        )}
        <div
          data-nextjs-scroll-focus-boundary
          className={classNames(
            list && isDetail && 'grow content min-h-0 overflow-y-auto',
            list && !isDetail && 'hidden lg:flex',
            !list && 'relative h-full grow overflow-y-auto content',
            'scrollbar',
          )}
        >
          {children}
    <div className="col-full-width content bg-stone-800 text-white py-12">
        <div className="mt-4 col-span-2 flex flex-wrap justify-between">
          <div>© 2023 Rozpisovnik.cz</div>
          <div>
            <div>Realizace: Jakub Zárybnický</div>
            <div>Verze: {process.env.BUILD_ID?.substring(0, 7)}</div>
          </div>
      </div>
    </div>
        </div>
      </div>
    </div>
  );
}

type Props = {
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu?: boolean;
};

export const Header = ({ isOpen, setIsOpen, showTopMenu }: Props) => {
  const topMenu = useTopMenu();
  const auth = useAuth();

  return (
    <div className="static w-full text-white bg-red-500 shadow-lg border-b border-red-700">
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
          <MobileLogo />
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

  const button = (
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
  );
  return (
    <Dropdown
      align="end"
      button={button}
      options={(memberMenu as DropdownItem[]).concat([
        { title: 'Odhlásit se', onClick: auth.signOut },
      ])}
    />
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
      ? 'text-white drop-shadow-xl border-b-[3px] tracking-wide -mb-[1px]'
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
    <Dropdown
      align="center"
      options={x.children}
      button={
        <button className={'block ' + cx}>
          {x.title} <ChevronDown className="w-4 h-4" />
        </button>
      }
    />
  );
};

type SidebarProps = {
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu?: boolean;
};

export const Sidebar = ({ isOpen, setIsOpen, showTopMenu }: SidebarProps) => {
  const router = useRouter();
  const topMenu = useTopMenu();
  const sideMenu = useSideMenu();
  const memberMenu = useMemberMenu();
  const auth = useAuth();
  React.useEffect(() => {
    const track = () => setIsOpen(false);
    router.events.on('routeChangeStart', track);
    return () => router.events.off('routeChangeStart', track);
  }, [router.events, setIsOpen]);

  React.useEffect(() => {
    if (typeof window === 'undefined') return;
    const updateDetailView = () => {
      if (window.matchMedia('(min-width: 768px)').matches) {
        setIsOpen(false);
      }
    };
    updateDetailView();
    window.addEventListener('resize', updateDetailView);
    return () => window.removeEventListener('resize', updateDetailView);
  }, [setIsOpen]);

  return (
    <>
      <div
        onClick={() => setIsOpen(false)}
        className={classNames(
          `fixed inset-0 z-20 bg-black bg-opacity-10 transition duration-200 ease-in-out`,
          isOpen ? 'pointer-events-auto opacity-100' : 'pointer-events-none opacity-0',
        )}
      />

      <nav
        className={classNames(
          isOpen
            ? 'absolute inset-y-0 left-0 translate-x-0 shadow-lg'
            : 'absolute -translate-x-full',
          showTopMenu && 'lg:hidden',
          'w-3/4 sm:w-1/2 md:w-1/3 lg:w-56 xl:w-64 2xl:w-72 3xl:w-80',
          'z-30 h-full max-h-screen min-h-screen flex-none transform pb-10 transition duration-200 ease-in-out sm:pb-0 md:w-1/3 lg:relative lg:z-auto lg:translate-x-0',
          'bg-white lg:bg-red-500 lg:text-white',
          'overflow-y-auto scrollbar'
        )}
      >
        {!showTopMenu && (
          <div className="hidden lg:flex">
              <Link href="/" className="h-24 mt-3 p-2 bg-black mx-auto">LOGO</Link>
          </div>
        )}
        <div className="space-y-1 pt-3 mr-1">
          {auth.user ? (
            <>
              <SidebarSection
                item={{ type: 'menu', title: 'Pro členy', children: memberMenu }}
              />
              <SidebarLink
                item={{ type: 'link', title: 'Odhlásit se', href: '/' }}
                onClick={auth.signOut}
              />
            </>
          ) : (
            <SidebarLink item={{ type: 'link', title: 'Přihlásit se', href: '/login' }} />
          )}
          {sideMenu.map((x) => (
            <SidebarSection key={x.title} item={x} />
          ))}
          {auth.user && <div className="h-8" />}
          {showTopMenu ? (
            topMenu.map((x) => <SidebarSection key={x.title} item={x} />)
          ) : (
            <SidebarLink item={{ type: 'link', title: 'Pro veřejnost', href: '/' }} />
          )}

          <div className="mt-4 text-xs text-stone-700 lg:text-white p-4 grid gap-2">
            <div>© 2023 Rozpisovnik.cz</div>
            <div>Verze: {process.env.BUILD_ID?.substring(0, 7)}</div>
          </div>
        </div>
      </nav>
    </>
  );
};

type SidebarLinkProps = {
  item: MenuLink;
  onClick?: React.MouseEventHandler<HTMLAnchorElement>;
};

const SidebarLink = ({ item, onClick }: SidebarLinkProps) => {
  const { pathname } = useRouter();
  const realHref = typeof item.href === 'string' ? item.href : route(item.href as Route);
  const inPath = pathname.startsWith(realHref) && realHref !== '/';

  return (
    <Link
      href={item.href}
      onClick={onClick}
      className={classNames(
        'rounded-2xl px-3 py-1.5',
        'flex items-center grow mx-2 hover:bg-red-700 hover:text-white',
        'tracking-wider text-sm',
        inPath ? 'underline font-bold bg-stone-700 text-white lg:bg-red-900' : '',
      )}
    >
      {item.title}
    </Link>
  );
};

const SidebarSection = ({ item }: { item: MenuStructItem }) => {
  return item.type === 'link' ? (
    <SidebarLink item={item} />
  ) : (
    <>
      <div key={item.title} className="ml-5 mt-4">
        <div className="font-bold text-xs uppercase grow my-1">{item.title}</div>
      </div>
      <div className="list-none grid gap-0.5">
        {item.children.map((y) => (
          <SidebarLink key={y.title} item={y} />
        ))}
      </div>
    </>
  );
};
