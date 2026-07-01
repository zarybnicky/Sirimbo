'use client';

import 'core-js/actual/array/to-reversed';
import 'core-js/actual/array/to-sorted';

import { buildId } from '@/lib/build-id';
import { cn } from '@/lib/cn';
import { configureUrql } from '@/lib/query';
import {
  getHrefs,
  type MenuLink,
  type MenuStructItem,
  topMenu,
  useMemberMenu,
} from '@/lib/use-menu';
import { hostToTenantId, serverTenantCatalog } from '@/tenant/catalog-server';
import { AuthButton } from '@/ui/AuthButton';
import { CallToAction } from '@/ui/CallToAction';
import { ConfirmProvider } from '@/ui/Confirm';
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuLink,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { ErrorNotifier } from '@/ui/ErrorNotifier';
import { FillYourProfileReminder } from '@/ui/FillYourProfileReminder';
import { buttonCls } from '@/ui/style';
import {
  authAtom,
  setNewTenant,
  storeRef,
  tenantConfigAtom,
  tenantIdAtom,
} from '@/ui/state/auth';
import { UpdateNotifier } from '@/ui/UpdateNotifier';
import { useAuth, UserRefresher } from '@/ui/use-auth';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { getCookie, setCookie } from 'cookies-next/client';
import { createStore, Provider as JotaiProvider, useAtom, useAtomValue, useSetAtom } from 'jotai';
import { ChevronDown, Menu as MenuIcon, User as Account } from 'lucide-react';
import Link, { type LinkProps } from 'next/link';
import { usePathname, useSearchParams } from 'next/navigation';
import { GoogleAnalytics, pageView as googlePageView } from 'nextjs-google-analytics';
import React from 'react';
import type { init, pageView as facebookPageView } from 'react-facebook-pixel';
import { ToastContainer } from 'react-toastify';
import { createClient, Provider as UrqlProvider } from 'urql';

type PublicChromeProps = {
  children: React.ReactNode;
  tenantId: string;
  desktopLogo: React.ReactNode;
  mobileLogo: React.ReactNode;
  sidebarLogo: React.ReactNode;
  socialIcons: React.ReactNode;
  footer: React.ReactNode;
};

export function PublicChrome({ children, tenantId, ...chrome }: PublicChromeProps) {
  const [store] = React.useState(() => createStore());
  const [client, setClient] = React.useState(() => createClient(configureUrql()));
  const resetUrqlClient = React.useCallback(() => {
    setClient(createClient(configureUrql()));
  }, []);

  // eslint-disable-next-line react-hooks/immutability
  storeRef.current = store;
  // eslint-disable-next-line react-hooks/immutability
  storeRef.resetUrqlClient = resetUrqlClient;

  return (
    <JotaiProvider store={store}>
      <UrqlProvider value={client}>
        <ConfirmProvider>
          <TenantCookieSync fallbackTenantId={tenantId} />
          <AppTracking />
          <PublicChromeContent {...chrome}>{children}</PublicChromeContent>
          <UpdateNotifier />
          <FillYourProfileReminder />
          <ErrorNotifier />
          <UserRefresher />
          <ToastContainer limit={3} />
        </ConfirmProvider>
      </UrqlProvider>
    </JotaiProvider>
  );
}

function TenantCookieSync({ fallbackTenantId }: { fallbackTenantId: string }) {
  useLayoutEffect(() => {
    const origin = new URL(window.origin);
    const tenantId = hostToTenantId.get(origin.hostname) ?? fallbackTenantId;
    const existing = String(getCookie('tenant_id'));
    const domain =
      origin.hostname === 'localhost' || origin.hostname === '127.0.0.1'
        ? undefined
        : origin.hostname;

    if (tenantId !== existing) {
      setCookie('tenant_id', tenantId, {
        path: '/',
        sameSite: 'lax',
        secure: origin.protocol === 'https:',
        domain,
        expires: new Date(Date.now() + 1000 * 60 * 60 * 24 * 365 * 10),
      });
    }
    setNewTenant(tenantId);
  }, [fallbackTenantId]);

  return null;
}

function PublicChromeContent({
  children,
  desktopLogo,
  mobileLogo,
  sidebarLogo,
  socialIcons,
  footer,
}: Omit<PublicChromeProps, 'tenantId'>) {
  const [isOpen, setIsOpen] = React.useState(false);
  const pathname = usePathname() || '/';
  const searchParams = useSearchParams();
  const { enableHome } = useAtomValue(tenantConfigAtom);
  const showTopMenu = enableHome;
  const search = searchParams?.toString() ?? '';
  const currentUrl = search ? `${pathname}?${search}` : pathname;

  return (
    <>
      <Header
        isOpen={isOpen}
        setIsOpen={setIsOpen}
        showTopMenu={showTopMenu}
        desktopLogo={desktopLogo}
        mobileLogo={mobileLogo}
        socialIcons={socialIcons}
      />

      <div className="flex min-h-[calc(100dvh-52px)] md:min-h-[calc(100dvh-68px)]">
        <Sidebar
          isOpen={isOpen}
          setIsOpen={setIsOpen}
          showTopMenu={showTopMenu}
          sidebarLogo={sidebarLogo}
        />

        <div className="grow content relative content-start">
          {children}
          {showTopMenu && (
            <>
              <CallToAction url={currentUrl} />
              {footer}
            </>
          )}
        </div>
      </div>
    </>
  );
}

function Header({
  isOpen,
  setIsOpen,
  showTopMenu,
  desktopLogo,
  mobileLogo,
  socialIcons,
}: {
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu: boolean;
  desktopLogo: React.ReactNode;
  mobileLogo: React.ReactNode;
  socialIcons: React.ReactNode;
}) {
  const auth = useAuth();
  const [isMounted, setIsMounted] = React.useState(false);

  React.useEffect(() => {
    setIsMounted(true);
  }, []);

  return (
    <div className="sticky z-20 top-0 inset-x-0 text-white bg-[#292524] shadow-lg">
      <div className="lg:container lg:max-w-6xl relative">
        {showTopMenu && (
          <div className="relative hidden lg:flex items-stretch justify-between min-h-[48px] md:min-h-[64px]">
            {desktopLogo}
            {topMenu.map((item) => (
              <DesktopMenuItem key={item.title} item={item} />
            ))}
            <AuthButton />
            {socialIcons}
          </div>
        )}

        <div className="flex lg:hidden items-stretch justify-between min-h-[48px] md:min-h-[64px] p-2">
          <button
            type="button"
            aria-label="Otevřít menu"
            className={buttonCls({ className: 'm-1', size: 'lg', variant: 'none' })}
            onClick={() => setIsOpen(!isOpen)}
          >
            <MenuIcon />
          </button>

          <div className="grow flex items-center">{mobileLogo}</div>

          <Link
            className={buttonCls({ className: 'm-1', size: 'lg', variant: 'none' })}
            href={auth.user && isMounted ? '/profil' : '/login'}
          >
            <Account />
          </Link>
        </div>
      </div>
    </div>
  );
}

function DesktopMenuItem({ item }: { item: MenuStructItem }) {
  const pathname = usePathname() || '/';
  const inPath = isItemInPath(item, pathname);

  const classes = cn(
    'flex gap-1 rounded-none transition-colors',
    'uppercase text-sm font-bold justify-center items-center',
    'hover:text-white hover:border-b-[3px] border-white data-[state=open]:border-b-[3px]',
    inPath
      ? 'text-white drop-shadow-xl border-b-[3px] tracking-wide -mb-px'
      : 'text-[#f3f3f3] drop-shadow',
  );

  if (item.type === 'link') {
    return (
      <Link href={item.href} className={`${classes} ${item.className}`}>
        {item.title}
      </Link>
    );
  }

  return (
    <DropdownMenu>
      <DropdownMenuTrigger className={`block ${classes}`}>
        {item.title} <ChevronDown className="size-4" />
      </DropdownMenuTrigger>
      <DropdownMenuContent align="center">
        {item.children.map((child) => (
          <DropdownMenuLink key={JSON.stringify(child.href)} href={child.href}>
            {child.title}
          </DropdownMenuLink>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}

function Sidebar({
  isOpen,
  setIsOpen,
  showTopMenu,
  sidebarLogo,
}: {
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu: boolean;
  sidebarLogo: React.ReactNode;
}) {
  const pathname = usePathname() || '/';
  const auth = useAuth();
  const setAuth = useSetAtom(authAtom);
  const tenantId = useAtomValue(tenantIdAtom);
  const { enableHome, copyrightLine: newCopyrightLine } =
    useAtomValue(tenantConfigAtom);
  const memberMenu = useMemberMenu();
  const [copyrightLine, setCopyrightLine] = React.useState('');
  const [isMounted, setIsMounted] = React.useState(false);

  React.useEffect(() => {
    setIsMounted(true);
    setCopyrightLine(newCopyrightLine);
  }, [newCopyrightLine]);

  React.useEffect(() => {
    setIsOpen(false);
  }, [pathname, setIsOpen]);

  React.useEffect(() => {
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
          'fixed inset-0 z-20 bg-neutral-12/10 transition-opacity duration-200 ease-in-out',
          isOpen ? 'pointer-events-auto opacity-100' : 'pointer-events-none opacity-0',
        )}
      />

      <nav
        className={cn(
          'fixed lg:sticky inset-y-0 left-0',
          isOpen ? 'translate-x-0 shadow-lg' : '-translate-x-full lg:translate-x-0',
          showTopMenu ? 'lg:hidden' : '',
          'w-3/4 sm:w-1/2 md:w-1/3 lg:w-56 xl:w-64 2xl:w-72 3xl:w-80',
          'z-50 lg:z-auto flex-none transition-transform duration-200 ease-in-out',
          'bg-accent-1 text-neutral-12 lg:bg-accent-9 lg:text-white',
          'overflow-y-auto scrollbar max-h-screen min-h-screen',
        )}
      >
        {!showTopMenu && sidebarLogo}
        <div className="space-y-1 pt-3 mr-1">
          {auth.user && isMounted ? (
            <>
              {memberMenu
                .map((item) =>
                  item.type === 'link'
                    ? item
                    : {
                        ...item,
                        children: item.children.filter(
                          (child) =>
                            (!child.requireTrainer || auth.isTrainerOrAdmin) &&
                            (!child.requireAdmin || auth.isAdmin) &&
                            (!child.requireSystemAdmin || auth.isSystemAdmin),
                        ),
                      },
                )
                .filter((item): item is MenuStructItem =>
                  item.type === 'link'
                    ? (!item.requireTrainer || auth.isTrainerOrAdmin) &&
                      (!item.requireAdmin || auth.isAdmin) &&
                      (!item.requireSystemAdmin || auth.isSystemAdmin)
                    : item.children.length > 0,
                )
                .map((item) => (
                  <SidebarSection key={item.title} item={item} pathname={pathname} />
                ))}

              <Link
                onClick={signOut}
                href={enableHome ? '/' : '/dashboard'}
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
            <SidebarLink
              pathname={pathname}
              item={{ type: 'link', title: 'Přihlásit se', href: '/login' }}
            />
          )}

          {enableHome &&
            (showTopMenu ? (
              topMenu.map((item) => (
                <SidebarSection key={item.title} item={item} pathname={pathname} />
              ))
            ) : (
              <SidebarLink
                pathname={pathname}
                item={{ type: 'link', title: 'Veřejná sekce', href: '/' }}
              />
            ))}

          <div className="mt-4 text-xs text-neutral-11 lg:text-white p-4 grid gap-2">
            <div>{copyrightLine}</div>
            <div>Verze: {buildId?.slice(0, 7)}</div>
            <div>
              <Link href="/now" target="_blank">
                Právě probíhá ↗︎
              </Link>
            </div>
            {isMounted && auth.isSystemAdmin && (
              <TenantSelect tenantId={tenantId} />
            )}
          </div>
        </div>
      </nav>
    </>
  );
}

function TenantSelect({ tenantId }: { tenantId: string }) {
  const [, setTenantId] = useAtom(tenantIdAtom);
  const onChange = React.useCallback(
    (e: React.ChangeEvent<HTMLSelectElement>) => {
      storeRef.resetUrqlClient();
      setTenantId(e.currentTarget.value);
    },
    [setTenantId],
  );

  return (
    <select className="text-neutral-12" onChange={onChange} value={tenantId}>
      {Object.values(serverTenantCatalog).map((tenant) => (
        <option key={tenant.id} value={tenant.id}>
          {tenant.name}
        </option>
      ))}
    </select>
  );
}

function SidebarLink({
  item,
  pathname,
  onClick,
}: {
  item: MenuLink;
  pathname: string;
  onClick?: React.MouseEventHandler<HTMLAnchorElement>;
}) {
  const inPath = isItemInPath(item, pathname, { rootMatches: false });

  return (
    <Link
      href={item.href}
      onClick={onClick}
      className={cn(
        'rounded-2xl px-3 py-1.5',
        'flex items-center grow mx-2 hover:bg-accent-10 hover:text-white',
        'tracking-wider text-sm',
        inPath ? 'underline font-bold bg-neutral-11 text-white lg:bg-accent-10' : '',
        item.className,
      )}
    >
      {item.title}
    </Link>
  );
}

function SidebarSection({
  item,
  pathname,
}: {
  item: MenuStructItem;
  pathname: string;
}) {
  return item.type === 'link' ? (
    <SidebarLink item={item} pathname={pathname} />
  ) : item.children.length > 0 ? (
    <>
      <div key={item.title} className="ml-5">
        <div className="font-bold text-xs uppercase grow mt-4">{item.title}</div>
      </div>
      <div className="list-none grid gap-0.5 pb-2">
        {item.children.map((child) => (
          <SidebarLink key={child.title} item={child} pathname={pathname} />
        ))}
      </div>
    </>
  ) : null;
}

function isItemInPath(
  item: MenuStructItem,
  pathname: string,
  { rootMatches = true }: { rootMatches?: boolean } = {},
) {
  return getHrefs(item).some((href) => {
    const hrefPath = hrefPathname(href);
    if (!hrefPath) return false;
    if (hrefPath === '/') return rootMatches && pathname === '/';
    return pathname.startsWith(hrefPath);
  });
}

function hrefPathname(href: LinkProps['href']) {
  if (typeof href === 'string') {
    return href.startsWith('/') ? href : '';
  }
  if (typeof href === 'object' && 'pathname' in href) {
    return href.pathname ?? '';
  }
  return '';
}

function AppTracking() {
  const pathname = usePathname() || '/';
  const searchParams = useSearchParams();
  const { facebookPixelId } = useAtomValue(tenantConfigAtom);
  const lastTrackedPath = React.useRef<string>(undefined);
  const facebookRef = React.useRef<{
    init: typeof init;
    pageView: typeof facebookPageView;
  } | null>(null);
  const search = searchParams?.toString() ?? '';
  const path = search ? `${pathname}?${search}` : pathname;

  React.useEffect(() => {
    if (process.env.NODE_ENV === 'development') return;
    if (!facebookPixelId) return;
    let disposed = false;
    (async () => {
      const facebook = await import('react-facebook-pixel').then((x) => x.default);
      if (disposed) return;
      facebook.init(facebookPixelId);
      facebook.pageView();
      facebookRef.current = facebook;
    })();
    return () => {
      disposed = true;
      facebookRef.current = null;
    };
  }, [facebookPixelId]);

  React.useEffect(() => {
    if (process.env.NODE_ENV === 'development') return;
    if (lastTrackedPath.current === undefined) {
      lastTrackedPath.current = path;
      return;
    }
    if (lastTrackedPath.current === path) return;
    lastTrackedPath.current = path;
    googlePageView({ path });
    facebookRef.current?.pageView();
  }, [path]);

  return <GoogleAnalytics trackPageViews={false} />;
}
