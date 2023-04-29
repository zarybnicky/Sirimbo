import classNames from 'classnames';
import { OlympLogoOneline } from 'components/Icons';
import { useAuth } from 'lib/data/use-auth';
import { MenuLink, MenuStructItem, useMemberMenu, useSideMenu } from 'lib/data/use-menu';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';

export const Sidebar = ({
  isOpen,
  setIsOpen,
  showTopMenu,
}: {
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu?: boolean;
}) => {
  const router = useRouter();
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
      <nav
        className={classNames(
          isOpen
            ? 'absolute inset-y-0 left-0 translate-x-0 shadow-lg'
            : 'absolute -translate-x-full',
          showTopMenu && 'lg:hidden',
          'w-3/4 sm:w-1/2 md:w-1/3 lg:w-56 xl:w-64 2xl:w-72 3xl:w-80',
          'z-30 h-full max-h-screen min-h-screen flex-none transform pb-10 transition duration-200 ease-in-out sm:pb-0 md:w-1/3 lg:relative lg:z-auto lg:translate-x-0',
          'bg-white dark:bg-gray-900 lg:bg-red-500 lg:text-white',
        )}
      >
        {!showTopMenu && (
          <div className="hidden lg:flex">
            <Link href="/" className="h-20 p-3 bg-red-500 mx-auto">
              <OlympLogoOneline className="h-full w-full text-white !fill-white" />
            </Link>
          </div>
        )}
        <div
          className={classNames(
            'grid gap-1 pt-3 mr-1 oveflow-y-auto max-h-full',
            'scrollbar-thin scrollbar-track-transparent scrollbar-thumb-red-700/80 hover:scrollbar-thumb-red-700/90',
          )}
        >
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
          <SidebarLink item={{ type: 'link', title: 'Pro veřejnost', href: '/' }} />

          <div className="mt-4 text-xs text-stone-700 lg:text-white p-4 grid gap-2">
            <div>© 2023 Taneční klub Olymp Olomouc, z. s.</div>
            <div>Verze: {process.env.BUILD_ID?.substring(0, 7)}</div>
          </div>
        </div>
      </nav>

      <div
        onClick={() => setIsOpen(false)}
        className={classNames(
          `fixed inset-0 z-20 bg-black bg-opacity-10 transition duration-200 ease-in-out dark:bg-opacity-50`,
          isOpen ? 'pointer-events-auto opacity-100' : 'pointer-events-none opacity-0',
        )}
      />
    </>
  );
};

const SidebarLink = ({
  item,
  onClick,
}: {
  item: MenuLink;
  onClick?: React.MouseEventHandler<HTMLAnchorElement>;
}) => {
  const { pathname } = useRouter();
  const inPath = pathname.startsWith(item.href) && item.href !== '/';

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
