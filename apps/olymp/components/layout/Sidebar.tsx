import { OlympLogoVertical } from '@app/ui/Icons';
import { useAuth } from '@app/ui/use-auth';
import classNames from 'classnames';
import { MenuLink, MenuStructItem, memberMenu, topMenu } from 'lib/use-menu';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';
import { currentTenant } from '@app/config';

type SidebarProps = {
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu?: boolean;
};

export const Sidebar = ({ isOpen, setIsOpen, showTopMenu }: SidebarProps) => {
  const router = useRouter();
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
          `fixed inset-0 z-10 bg-black/10 transition duration-200 ease-in-out`,
          isOpen ? 'pointer-events-auto opacity-100' : 'pointer-events-none opacity-0',
        )}
      />

      <nav
        className={classNames(
          'absolute lg:sticky inset-y-0 left-0',
          isOpen
            ? 'translate-x-0 shadow-lg'
            : '-translate-x-full lg:translate-x-0',
          showTopMenu ? 'lg:hidden' : '',
          'w-3/4 sm:w-1/2 md:w-1/3 lg:w-56 xl:w-64 2xl:w-72 3xl:w-80',
          'z-30 lg:z-auto flex-none pb-10 transition duration-200 ease-in-out sm:pb-0',
          'bg-accent-1 lg:bg-primary lg:text-white',
          'overflow-y-auto scrollbar max-h-screen min-h-screen'
        )}
      >
        {!showTopMenu && (
          <div className="hidden lg:flex">
            <Link href="/" className="h-20 mt-3 mx-auto">
              <OlympLogoVertical className="h-full w-full text-white !fill-white" />
            </Link>
          </div>
        )}
        <div className="space-y-1 pt-3 mr-1">
          {auth.user ? (
            <>
              {memberMenu.map(x => x.type === 'link' ? x : {
                ...x,
                children: x.children.filter(item => (
                  !(item.requireTrainer && !auth.perms.isTrainer) &&
                  !(item.requireAdmin && !auth.perms.isTrainerOrAdmin))
                )
              }).map((x) => <SidebarSection key={x.title} item={x} />)}

              <SidebarLink
                item={{ type: 'link', title: 'Odhlásit se', href: '/' }}
                onClick={auth.signOut}
              />
              <div className="h-8" />
            </>
          ) : (
            <SidebarLink item={{ type: 'link', title: 'Přihlásit se', href: '/login' }} />
          )}

          {currentTenant.enableHome && (
            showTopMenu ? (
              topMenu.map((x) => <SidebarSection key={x.title} item={x} />)
            ) : (
              <SidebarLink item={{ type: 'link', title: 'Veřejná sekce', href: '/' }} />
            )
          )}

          <div className="mt-4 text-xs text-stone-700 lg:text-white p-4 grid gap-2">
            <div>{currentTenant.copyrightLine}</div>
            <div>Verze: {(process.env.NEXT_PUBLIC_VERCEL_GIT_COMMIT_SHA || process.env.BUILD_ID)?.substring(0, 7)}</div>
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
  const inPath = pathname.startsWith(item.href) && item.href !== '/';

  return (
    <Link
      href={item.href}
      onClick={onClick}
      className={classNames(
        'rounded-2xl px-3 py-1.5',
        'flex items-center grow mx-2 hover:bg-accent-10 hover:text-white',
        'tracking-wider text-sm',
        inPath ? 'underline font-bold bg-stone-700 text-white lg:bg-accent-9' : '',
      )}
    >
      {item.title}
    </Link>
  );
};

const SidebarSection = ({ item }: { item: MenuStructItem }) => {
  return item.type === 'link' ? (
    <SidebarLink item={item} />
  ) : item.children.length > 0 ? (
    <>
      <div key={item.title} className="ml-5">
        <div className="font-bold text-xs uppercase grow mt-4">{item.title}</div>
      </div>
      <div className="list-none grid gap-0.5 pb-2">
        {item.children.map((y) => (
          <SidebarLink key={y.title} item={y} />
        ))}
      </div>
    </>
  ) : null;
};
