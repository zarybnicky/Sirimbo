import { signOut, useAuth, useTenantConfig, useTenantId } from '@/lib/auth';
import { buildId } from '@/lib/build-id';
import { cn } from '@/lib/cn';
import { clampSidebarWidth, sidebarWidthAtom } from '@/lib/ui';
import {
  type MenuLink,
  type MenuStructItem,
  topMenu,
  useMemberMenu,
} from '@/lib/use-menu';
import { getTenantUi } from '@/tenant/ui';
import { TenantSelect } from '@/ui/TenantSelect';
import { formatCount, moneyFormatter } from '@/ui/format';
import { useSetAtom } from 'jotai';
import Link from 'next/link';
import { usePathname, useSearchParams } from 'next/navigation';
import React from 'react';

type SidebarProps = {
  homeHref: string;
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu?: boolean;
};

export function Sidebar({ homeHref, isOpen, setIsOpen, showTopMenu }: SidebarProps) {
  const pathname = usePathname();
  const search = useSearchParams()?.toString();
  const auth = useAuth();
  const tenantId = useTenantId();
  const { publicSite, copyrightLine } = useTenantConfig();
  const memberMenu = useMemberMenu();
  const { SidebarLogo } = getTenantUi(tenantId);

  React.useEffect(() => setIsOpen(false), [pathname, search, setIsOpen]);

  React.useEffect(() => {
    if (typeof window === 'undefined') return;
    const desktop = window.matchMedia('(min-width: 64rem)');
    const closeOnDesktop = () => {
      if (desktop.matches) setIsOpen(false);
    };
    closeOnDesktop();
    desktop.addEventListener('change', closeOnDesktop);
    return () => desktop.removeEventListener('change', closeOnDesktop);
  }, [setIsOpen]);

  return (
    <>
      <div
        onClick={() => setIsOpen(false)}
        className={cn(
          'fixed print:hidden inset-0 z-20 bg-neutral-12/10 transition-opacity duration-200 ease-in-out',
          isOpen ? 'pointer-events-auto opacity-100' : 'pointer-events-none opacity-0',
        )}
      />

      <div
        className={cn(
          'fixed lg:sticky inset-y-0 left-0',
          isOpen ? 'translate-x-0 shadow-lg' : '-translate-x-full lg:translate-x-0',
          showTopMenu ? 'lg:hidden' : '',
          'w-3/4 sm:w-1/2 md:w-1/3',
          'lg:w-(--sidebar-width,14rem) xl:w-(--sidebar-width,16rem)',
          '2xl:w-(--sidebar-width,18rem) min-[1920px]:w-(--sidebar-width,20rem)',
          'z-50 lg:z-10 flex-none transition-transform duration-200 ease-in-out',
          'bg-accent-1 text-neutral-12 lg:bg-accent-9 lg:text-white',
          'max-h-screen min-h-screen',
        )}
      >
        <nav id="app-navigation" className="h-screen overflow-y-auto scrollbar">
          {!showTopMenu && <SidebarLogo href={homeHref} />}
          <div className="space-y-1 pt-3 mr-1 relative">
            {auth.user ? (
              <>
                <SidebarIdentity />
                {memberMenu.map((item) => (
                  <SidebarSection key={item.title} item={item} />
                ))}

                <div className="w-full flex px-2">
                  <button
                    type="button"
                    onClick={signOut}
                    className={cn(
                      'flex items-center flex-1 px-3 py-1.5 rounded-2xl',
                      'text-sm tracking-wider hover:bg-accent-10 hover:text-white',
                    )}
                  >
                    Odhlásit se
                  </button>
                </div>
                <div className="h-8" />
              </>
            ) : (
              <SidebarLink
                item={{ type: 'link', title: 'Přihlásit se', href: '/login' }}
              />
            )}

            {publicSite &&
              (showTopMenu ? (
                topMenu.map((item) => <SidebarSection key={item.title} item={item} />)
              ) : (
                <SidebarLink item={{ type: 'link', title: 'Veřejná sekce', href: '/' }} />
              ))}

            <div className="mt-4 text-xs text-neutral-11 lg:text-white p-4 grid gap-2">
              <div>{copyrightLine}</div>
              <div>Verze: {buildId?.slice(0, 7)}</div>
              <div>
                <Link href="/now" target="_blank">
                  Právě probíhá ↗︎
                </Link>
              </div>
              <TenantSelect />
            </div>
          </div>
        </nav>

        <SidebarResizeHandle />
      </div>
    </>
  );
}

function SidebarIdentity() {
  const { user, persons } = useAuth();
  const pathname = usePathname();
  const searchParams = useSearchParams();
  const [now] = React.useState(Date.now);
  const applications = user?.pendingApplications ?? [];
  const selectedPerson =
    searchParams?.get('person') ??
    persons[0]?.id ??
    (applications.length > 0 ? 'applications' : null);
  const selectedApplication = searchParams?.get('application') ?? applications[0]?.id;

  return (
    <div className="grid gap-1.5 pb-3">
      {persons.map((person) => {
        const unpaidCount = person.unpaidPayments.length;
        const overdueCount = person.unpaidPayments.filter(
          ({ payment }) => payment?.dueAt && new Date(payment.dueAt).getTime() < now,
        ).length;

        return (
          <SidebarLink
            key={person.id}
            selected={pathname === '/profil' && selectedPerson === person.id}
            item={{
              type: 'link',
              title: person.name,
              href: `/profil?person=${person.id}`,
              className: 'rounded-xl py-2 tracking-normal',
            }}
            subtitle={
              <div className="flex flex-col gap-1 no-underline">
                {person.accountsList.length > 0
                  ? `Kredit ${person.accountsList.map((x) => moneyFormatter.format(x)).join(' · ')}`
                  : null}
                {unpaidCount > 0 &&
                  [
                    unpaidCount > overdueCount &&
                      formatCount(unpaidCount - overdueCount, {
                        one: 'čekající platba',
                        few: 'čekající platby',
                        other: 'čekající plateb',
                      }),
                    overdueCount && `${overdueCount} po splatnosti`,
                  ]
                    .filter(Boolean)
                    .join(', ')}
              </div>
            }
          />
        );
      })}
      {persons.length === 0 && (
        <SidebarLink
          item={{
            type: 'link',
            title: user?.uEmail ?? '',
            href: '/profil',
            className: 'rounded-xl py-2 tracking-normal',
          }}
          selected={pathname === '/profil' && applications.length === 0}
        />
      )}
      {applications.map((application) => (
        <SidebarLink
          key={application.id}
          item={{
            type: 'link',
            title: `${application.firstName} ${application.lastName}`,
            href: `/profil?person=applications&application=${application.id}`,
            className: 'rounded-xl py-2 tracking-normal',
          }}
          subtitle="Čeká na schválení"
          selected={
            pathname === '/profil' &&
            selectedPerson === 'applications' &&
            selectedApplication === application.id
          }
        />
      ))}
    </div>
  );
}

function SidebarResizeHandle() {
  const setWidth = useSetAtom(sidebarWidthAtom);
  const drag = React.useRef<{
    pointerId: number;
    startX: number;
    startWidth: number;
    width: number | null;
    sidebar: HTMLElement;
  } | null>(null);

  const finishDrag = (event: React.PointerEvent<HTMLDivElement>) => {
    const current = drag.current;
    if (current?.pointerId !== event.pointerId) return;
    if (current.width !== null) setWidth(current.width);
    drag.current = null;
    if (event.currentTarget.hasPointerCapture(event.pointerId)) {
      event.currentTarget.releasePointerCapture(event.pointerId);
    }
    event.currentTarget.blur();
  };

  return (
    <div
      role="separator"
      aria-orientation="vertical"
      tabIndex={0}
      className={cn(
        'group absolute inset-y-0 left-full ml-0.5 hidden w-2 lg:block',
        'touch-none select-none cursor-col-resize outline-hidden',
      )}
      onPointerDown={(event) => {
        if (event.button !== 0) return;
        const parent = event.currentTarget.parentElement;
        if (!parent) return;

        event.preventDefault();
        event.currentTarget.focus();
        event.currentTarget.setPointerCapture(event.pointerId);
        drag.current = {
          pointerId: event.pointerId,
          startX: event.clientX,
          startWidth: parent.getBoundingClientRect().width,
          width: null,
          sidebar: parent,
        };
      }}
      onPointerMove={(event) => {
        if (drag.current?.pointerId !== event.pointerId) return;
        const width = clampSidebarWidth(
          drag.current.startWidth + event.clientX - drag.current.startX,
        );
        drag.current.width = width;
        drag.current.sidebar.style.setProperty('--sidebar-width', `${width}px`);
      }}
      onPointerUp={finishDrag}
      onPointerCancel={finishDrag}
    >
      <span
        aria-hidden="true"
        className={cn(
          'absolute inset-y-0 left-0 w-0.5 bg-accent-9 opacity-0 transition-opacity',
          'group-hover:opacity-100 group-active:opacity-100 group-focus-visible:opacity-100',
        )}
      />
    </div>
  );
}

type SidebarLinkProps = {
  item: MenuLink;
  onClick?: React.MouseEventHandler<HTMLAnchorElement>;
  subtitle?: React.ReactNode;
  selected?: boolean;
};

function SidebarLink({ item, onClick, subtitle, selected }: SidebarLinkProps) {
  const pathname = usePathname() ?? '';
  const searchParams = useSearchParams();
  const [href, query] = item.href.split('?');
  const queryMatches = query
    ? [...new URLSearchParams(query)].every(
        ([name, value]) => searchParams?.get(name) === value,
      )
    : !searchParams?.has('tab');
  const inPath =
    selected ??
    (!!href &&
      (href === '/'
        ? pathname === '/'
        : pathname === href || pathname.startsWith(`${href}/`)) &&
      queryMatches);

  return (
    <Link
      href={item.href}
      onClick={onClick}
      aria-current={inPath ? 'page' : undefined}
      className={cn(
        'flex items-center flex-1 mx-2 px-3 py-1.5 rounded-2xl',
        'text-sm tracking-wider hover:bg-accent-10 hover:text-white',
        item.className,
        inPath ? 'font-bold bg-neutral-11 text-white lg:bg-accent-10' : '',
      )}
    >
      <span className="min-w-0">
        <span className={cn('block wrap-anywhere', inPath && 'underline')}>{item.title}</span>
        {subtitle && (
          <span className="mt-0.5 block text-xs font-normal opacity-80">{subtitle}</span>
        )}
      </span>
    </Link>
  );
}

function SidebarSection({ item }: { item: MenuStructItem }) {
  if (item.type === 'link') {
    return <SidebarLink item={item} />;
  }
  if (item.children.length <= 0) {
    return null;
  }
  return (
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
  );
}
