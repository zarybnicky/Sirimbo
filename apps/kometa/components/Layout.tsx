import classNames from 'classnames';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';
import { Menu as MenuIcon } from 'lucide-react';
import { useAuth } from '@app/ui/use-auth';
import { Route, route } from 'nextjs-routes';
import { origin } from '@app/graphql/query';

export type MenuLink = {
  type: 'link';
  title: string;
  href: Route | Exclude<Route, { query: any }>['pathname'];
};

export type MenuStructItem =
  | {
      type: 'menu';
      title: string;
      children: MenuLink[];
    }
  | MenuLink;

export type LayoutProps = {
  list?: React.ReactNode;
  isDetail?: boolean;
  children?: React.ReactNode;
};

export const menu: MenuLink[] = [
  { type: 'link', title: 'Kalendář', href: '/' },
  { type: 'link', title: 'Členové', href: '/members' },
  { type: 'link', title: 'Klub', href: '/club' },
];

export function Layout({ children, list, isDetail }: LayoutProps) {
  const [isOpen, setIsOpen] = React.useState(false);
  const router = useRouter();
  const auth = useAuth();

  React.useEffect(() => {
    const track = () => setIsOpen(false);
    router.events.on('routeChangeStart', track);
    return () => router.events.off('routeChangeStart', track);
  }, [router.events]);

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
  }, []);

  React.useEffect(() => {
    const scroll = () =>
      document.querySelector('[data-nextjs-scroll-focus-boundary]')?.scrollTo(0, 0);
    router.events.on('routeChangeComplete', scroll);
    return () => router.events.off('routeChangeComplete', scroll);
  }, [router.events]);

  return (
    <div className="h-screen flex flex-col w-full relative overflow-hidden">
      <div className="static w-full text-white bg-accent-9 shadow-lg border-b border-accent-10">
        <div className="container relative max-w-5xl mx-auto">
          <div className="flex lg:hidden items-stretch justify-between min-h-[48px] md:min-h-[64px] p-2">
            <button
              className="flex items-center button-icon p-0 m-1"
              onClick={() => setIsOpen((x) => !x)}
            >
              <MenuIcon className="w-5 h-5" />
            </button>
          </div>
        </div>
      </div>
      <div className="relative grow flex overflow-hidden bg-accent-2 text-accent-12">
        <div
          onClick={() => setIsOpen(false)}
          className={classNames(
            `fixed inset-0 z-20 bg-black/10 transition duration-200 ease-in-out`,
            isOpen ? 'pointer-events-auto opacity-100' : 'pointer-events-none opacity-0',
          )}
        />

        <nav
          className={classNames(
            isOpen
              ? 'absolute inset-y-0 left-0 translate-x-0 shadow-lg'
              : 'absolute -translate-x-full',
            'w-3/4 sm:w-1/2 md:w-1/3 lg:w-56 xl:w-64 2xl:w-72 3xl:w-80',
            'z-30 h-full max-h-screen min-h-screen flex-none pb-10 transition duration-200 ease-in-out sm:pb-0 lg:relative lg:z-auto lg:translate-x-0',
            'bg-white lg:bg-accent-9 lg:text-white',
            'overflow-y-auto scrollbar',
          )}
        >
          <div className="space-y-1 pt-3 mr-1">
            {menu.map((x) => (
              <SidebarSection key={x.title} item={x} />
            ))}
            {auth.user && (
              <SidebarLink
                item={{ type: 'link', title: 'Odhlásit se', href: '/' }}
                onClick={auth.signOut}
              />
            )}

            <div className="mt-4 text-xs text-neutral-10 lg:text-white p-4 grid gap-2">
              <div>© 2023 Rozpisovnik.cz</div>
              <div>API server: {origin}</div>
              <div>Verze: {(process.env.NEXT_PUBLIC_VERCEL_GIT_COMMIT_SHA || process.env.BUILD_ID)?.substring(0, 7)}</div>
            </div>
          </div>
        </nav>

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
        </div>
      </div>
    </div>
  );
}

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
        'flex items-center grow mx-2 hover:bg-accent-10 hover:text-white',
        'tracking-wider text-sm',
        inPath ? 'underline font-bold bg-accent-9 text-white' : '',
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
