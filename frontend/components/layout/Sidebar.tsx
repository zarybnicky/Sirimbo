import classNames from 'classnames';
import { OlympLogoOneline } from 'components/Icons';
import { useAuth } from 'lib/data/use-auth';
import { MenuLink, MenuStructItem, useSideMenu, useTopMenu } from 'lib/data/use-menu';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';

export const Sidebar = ({ isOpen, setIsOpen, showTopMenu }: {
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu: boolean;
}) => {
  const router = useRouter();
  const sideMenu = useSideMenu();
  const topMenu = useTopMenu();
  const auth = useAuth();

  React.useEffect(() => {
    const track = () => setIsOpen(false);
    router.events.on('routeChangeStart', track)
    return () => router.events.off('routeChangeStart', track);
  }, [router.events]);

  React.useEffect(() => {
    if (typeof window === 'undefined') return;
    const updateDetailView = () => {
      if (window.matchMedia("(min-width: 768px)").matches && isOpen) {
        setIsOpen(false);
      }
    };
    updateDetailView();
    window.addEventListener('resize', updateDetailView);
    return () => window.removeEventListener('resize', updateDetailView);
  }, []);

  return <>
    <nav className={classNames(
      isOpen ? 'absolute inset-y-0 left-0 translate-x-0 shadow-lg' : 'absolute -translate-x-full',
      "w-3/4 sm:w-1/2 md:w-1/3 lg:w-56 xl:w-64 2xl:w-72 3xl:w-80",
      "scrollbar-thin scrollbar-track-transparent scrollbar-thumb-stone-800/30 hover:scrollbar-thumb-stone-800/50",
      "z-30 h-full max-h-screen min-h-screen flex-none transform overflow-y-auto border-r border-gray-150 bg-white pb-10 transition duration-200 ease-in-out dark:border-gray-800 dark:bg-gray-900 sm:pb-0 md:w-1/3 lg:relative lg:z-auto lg:translate-x-0 lg:bg-gray-50 lg:dark:bg-gray-900",
    )}>
      <div className="grid gap-1 pt-3 mr-1">
        {!showTopMenu && (
          <div className="hidden lg:flex ml-4">
            <OlympLogoOneline viewBox="0 0 381.82217 111.78744" width="170" height="50" style={{
              color: 'black',
              fill: 'black !important',
            }} />
          </div>
        )}
        {sideMenu.map(x => <SidebarSection key={x.title} item={x} />)}
        <SidebarLink item={{ type: 'link', title: 'Domů', href: '/home' }} />
        {topMenu.map(x => <SidebarSection key={x.title} item={x} />)}

        {auth.user ? <>
          <SidebarLink item={{ type: 'link', title: 'Profil', href: '/profile' }} />
          <SidebarLink item={{ type: 'link', title: 'Odhlásit se', href: '/' }} onClick={() => {
            auth.signOut();
            router.push('/');
          }} />
        </> : (
          <SidebarLink item={{ type: 'link', title: 'Přihlásit se', href: '/login' }} />
        )}

        <div className="mt-4 text-xs text-slate-700 p-4 grid gap-2">
          <div>© 2022 Taneční klub Olymp Olomouc, z. s.</div>
          <div>Verze: {process.env.BUILD_ID?.substring(0, 7)}</div>
        </div>
      </div>
    </nav>

    <div onClick={() => setIsOpen(false)} className={classNames(
      `fixed inset-0 z-20 bg-black bg-opacity-10 transition duration-200 ease-in-out dark:bg-opacity-50`,
      isOpen ? 'pointer-events-auto opacity-100' : 'pointer-events-none opacity-0',
    )} />
  </>
};

export const SidebarLink = ({ item, onClick }: {
  item: MenuLink;
  onClick?: React.MouseEventHandler<HTMLAnchorElement>;
}) => {
  const { pathname } = useRouter();
  const inPath = pathname.startsWith(item.href);
  return (
    <Link href={item.href} passHref>
      <a onClick={onClick} className={classNames(
        "rounded-2xl px-3 py-1.5",
        "flex items-center grow mx-2 hover:bg-stone-500 hover:text-white",
        "font-light tracking-wide text-sm",
        inPath ? 'bg-stone-700 text-white' : '',
      )}>
        {item.title}
      </a>
    </Link>
  );
};

export const SidebarSection = ({ item }: { item: MenuStructItem; }) => {
  return item.type === 'link' ? <SidebarLink item={item} /> : <>
    <div key={item.title} className="ml-5 mt-2">
      <div className="font-bold text-xs uppercase grow my-1">{item.title}</div>
    </div>
    <div className="list-none grid gap-0.5 mb-2">
      {item.children.map(y => <SidebarLink key={y.title} item={y} />)}
    </div>
  </>;
};