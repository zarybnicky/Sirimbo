import { buildId } from '@/lib/build-id';
import { type MenuLink, type MenuStructItem, getHrefs, memberMenu, topMenu } from '@/lib/use-menu';
import { tenantConfig } from '@/tenant/config.js';
import { getTenantUi } from '@/tenant/catalog';
import { cn } from '@/ui/cn';
import { authAtom, storeRef } from '@/ui/state/auth';
import { useAuth } from '@/ui/use-auth';
import { useSetAtom } from 'jotai';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';
import dynamic from 'next/dynamic';

const SidebarLogo = dynamic(() => getTenantUi('SidebarLogo'));

type SidebarProps = {
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu?: boolean;
};

export function Sidebar({ isOpen, setIsOpen, showTopMenu }: SidebarProps) {
  const router = useRouter();
  const auth = useAuth();
  const setAuth = useSetAtom(authAtom);
  const hasPublicNavigation = topMenu.length > 0;
  const canAccessLink = React.useCallback(
    (link: MenuLink) =>
      (!link.requireTrainer || auth.isTrainerOrAdmin) && (!link.requireAdmin || auth.isAdmin),
    [auth.isAdmin, auth.isTrainerOrAdmin],
  );

  const [isMounted, setIsMounted] = React.useState(false);
  React.useEffect(() => {
    setIsMounted(true);
  }, []);

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

  const signOut = React.useCallback(() => {
    setAuth(null, null);
    storeRef.resetUrqlClient?.();
  }, [setAuth]);

  return (
    <>
      <div
        onClick={() => setIsOpen(false)}
        className={cn(
          "fixed inset-0 z-20 bg-black/10 transition duration-200 ease-in-out",
          isOpen ? 'pointer-events-auto opacity-100' : 'pointer-events-none opacity-0',
        )}
      />

      <nav
        className={cn(
          'fixed lg:sticky inset-y-0 left-0',
          isOpen
            ? 'translate-x-0 shadow-lg'
            : '-translate-x-full lg:translate-x-0',
          showTopMenu ? 'lg:hidden' : '',
          'w-3/4 sm:w-1/2 md:w-1/3 lg:w-56 xl:w-64 2xl:w-72 3xl:w-80',
          'z-50 lg:z-auto flex-none pb-10 transition duration-200 ease-in-out sm:pb-0',
          'bg-accent-1 lg:bg-primary lg:text-white',
          'overflow-y-auto scrollbar max-h-screen min-h-screen'
        )}
      >
        {!showTopMenu && (
          <SidebarLogo />
        )}
        <div className="space-y-1 pt-3 mr-1">
          {(auth.user && isMounted) ? (
            <>
              {memberMenu.map((x) => {
                if (x.type === 'link') {
                  return canAccessLink(x) ? (
                    <SidebarSection key={x.title} item={x} />
                  ) : null;
                }

                const filteredChildren = x.children.filter(canAccessLink);
                if (filteredChildren.length === 0) {
                  return null;
                }

                return (
                  <SidebarSection key={x.title} item={{ ...x, children: filteredChildren }} />
                );
              })}

              <Link
                onClick={signOut}
                href={tenantConfig.enableHome ? '/' : '/dashboard'}
                className={cn(
                  'rounded-2xl px-3 py-1.5',
                  'flex items-center grow mx-2 hover:bg-accent-10 hover:text-white',
                  'tracking-wider text-sm',
                )}
              >
                Odhlásit se
              </Link>
              <div className="h-8" />
            </>
          ) : (
            <SidebarLink item={{ type: 'link', title: 'Přihlásit se', href: '/login' }} />
          )}

          {tenantConfig.enableHome && hasPublicNavigation && (
            showTopMenu ? (
              topMenu.map((x) => <SidebarSection key={x.title} item={x} />)
            ) : (
              <SidebarLink item={{ type: 'link', title: 'Veřejná sekce', href: '/' }} />
            )
          )}

          <div className="mt-4 text-xs text-neutral-11 lg:text-white p-4 grid gap-2">
            <div>{tenantConfig.copyrightLine}</div>
            <div>Verze: {buildId?.slice(0, 7)}</div>
            <div>
              <Link href="/now" target="_blank">
                Právě probíhá ↗︎
              </Link>
            </div>
          </div>
        </div>
      </nav>
    </>
  );
}

type SidebarLinkProps = {
  item: MenuLink;
  onClick?: React.MouseEventHandler<HTMLAnchorElement>;
};

function SidebarLink({ item, onClick }: SidebarLinkProps) {
  const { pathname } = useRouter();

  const inPath = !!getHrefs(item).some((x) => {
    const y = typeof x === 'object' ? ('pathname' in x ? x.pathname : '') : x;
    return y === '/' ? false : pathname.startsWith(y);
  });

  return (
    <Link
      href={item.href}
      onClick={onClick}
      className={cn(
        'rounded-2xl px-3 py-1.5',
        'flex items-center grow mx-2 hover:bg-accent-10 hover:text-white',
        'tracking-wider text-sm',
        inPath ? 'underline font-bold bg-neutral-11 text-white lg:bg-accent-9' : '',
        item.className,
      )}
    >
      {item.title}
    </Link>
  );
}

function SidebarSection({ item }: { item: MenuStructItem }) {
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
}
