import { signOut, useAuth, useTenantConfig, useTenantId } from '@/lib/auth';
import { buildId } from '@/lib/build-id';
import { cn } from '@/lib/cn';
import { sidebarWidthAtom } from '@/lib/ui';
import {
  getHrefs,
  topMenu,
  useMemberMenu,
  type MenuLink,
  type MenuStructItem,
} from '@/lib/use-menu';
import { getTenantUi } from '@/tenant/ui';
import { TenantSelect } from '@/ui/TenantSelect';
import { useSetAtom } from 'jotai';
import Link from 'next/link';
import { usePathname } from 'next/navigation';
import React from 'react';

type SidebarProps = {
  isOpen: boolean;
  setIsOpen: React.Dispatch<React.SetStateAction<boolean>>;
  showTopMenu?: boolean;
};

export function Sidebar({ isOpen, setIsOpen, showTopMenu }: SidebarProps) {
  const pathname = usePathname();
  const auth = useAuth();
  const tenantId = useTenantId();
  const { publicSite, copyrightLine } = useTenantConfig();
  const memberMenu = useMemberMenu();
  const { SidebarLogo } = getTenantUi(tenantId);

  React.useEffect(() => setIsOpen(false), [pathname, setIsOpen]);

  React.useEffect(() => {
    if (typeof window === 'undefined') return;
    const updateDetailView = () => {
      if (window.matchMedia('(min-width: 768px)').matches) setIsOpen(false);
    };
    updateDetailView();
    window.addEventListener('resize', updateDetailView);
    return () => window.removeEventListener('resize', updateDetailView);
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
          'app-sidebar w-3/4 sm:w-1/2 md:w-1/3',
          'lg:w-[var(--sidebar-width,var(--sidebar-default-width))]',
          'z-50 lg:z-10 flex-none transition-transform duration-200 ease-in-out',
          'bg-accent-1 text-neutral-12 lg:bg-accent-9 lg:text-white',
          'max-h-screen min-h-screen',
        )}
      >
        <nav
          id="app-navigation"
          aria-label="Hlavní navigace"
          className="h-screen overflow-y-auto scrollbar"
        >
          {!showTopMenu && <SidebarLogo />}
          <div className="space-y-1 pt-3 mr-1 relative">
            {auth.user ? (
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

function SidebarResizeHandle() {
  const setWidth = useSetAtom(sidebarWidthAtom);
  const drag = React.useRef<{
    pointerId: number;
    startX: number;
    startWidth: number;
  } | null>(null);

  const finishDrag = (event: React.PointerEvent<HTMLDivElement>) => {
    if (drag.current?.pointerId !== event.pointerId) return;
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
        'touch-none select-none cursor-col-resize outline-none',
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
        };
      }}
      onPointerMove={(event) => {
        if (drag.current?.pointerId !== event.pointerId) return;
        setWidth(drag.current.startWidth + event.clientX - drag.current.startX);
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
};

function SidebarLink({ item, onClick }: SidebarLinkProps) {
  const pathname = usePathname() ?? '';
  const inPath = getHrefs(item).some((x) => {
    const y = typeof x === 'object' ? ('pathname' in x ? x.pathname : '') : x;
    if (!y) return false;
    return y === '/' ? false : pathname.startsWith(y);
  });

  return (
    <Link
      href={item.href}
      onClick={onClick}
      className={cn(
        'flex items-center flex-1 mx-2 px-3 py-1.5 rounded-2xl',
        'text-sm tracking-wider hover:bg-accent-10 hover:text-white',
        inPath ? 'underline font-bold bg-neutral-11 text-white lg:bg-accent-10' : '',
        item.className,
      )}
    >
      {item.title}
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
