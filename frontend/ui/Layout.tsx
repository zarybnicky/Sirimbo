'use client';

import { canAccess, type AccessRequirements } from '@/lib/auth-claims';
import { getTenantUi } from '@/tenant/ui';
import { ErrorPage } from '@/ui/ErrorPage';
import { useAuth, useAuthLoading, useTenantConfig, useTenantId } from '@/lib/auth';
import { CallToAction } from '@/ui/CallToAction';
import React from 'react';
import { Header } from '@/ui/Header';
import { Sidebar } from '@/ui/Sidebar';
import { useRouter, usePathname, useSearchParams } from 'next/navigation';

type LayoutProps = AccessRequirements & {
  hideTopMenuIfLoggedIn?: boolean;
  showTopMenu?: boolean;
  children?: React.ReactNode;
  hideCta?: boolean;
  className?: string;
};

export const Layout = React.memo(function Layout({
  children,
  showTopMenu,
  hideTopMenuIfLoggedIn,
  hideCta,
  className,
  ...requirements
}: LayoutProps) {
  const [isOpen, setIsOpen] = React.useState(false);
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const tenantId = useTenantId();
  const tenant = useTenantConfig();
  const { Footer } = getTenantUi(tenantId);

  const router = useRouter();
  const search = useSearchParams()?.toString();
  const url = usePathname() + (search ? `?${search}` : '');

  const missingPermission = !canAccess(auth, tenant, requirements);

  React.useEffect(() => {
    if (!authLoading && missingPermission && !auth.user) {
      router.push(`/login?from=${encodeURIComponent(url)}`);
    }
  }, [router, auth.user, authLoading, missingPermission, url]);

  showTopMenu = tenant.publicSite ? showTopMenu : false;
  if (hideTopMenuIfLoggedIn) {
    showTopMenu = !!tenant.publicSite && !auth.user;
  }
  if (missingPermission) {
    children = authLoading ? null : auth.user ? (
      <ErrorPage
        error="Přístup zamítnut"
        details="Nemáte dostatečná práva pro zobrazení této stránky"
      />
    ) : null;
  }

  return (
    <>
      <Header isOpen={isOpen} setIsOpen={setIsOpen} showTopMenu={showTopMenu} />

      <div className="flex min-h-[calc(100dvh-52px)] md:min-h-[calc(100dvh-68px)]">
        <Sidebar isOpen={isOpen} setIsOpen={setIsOpen} showTopMenu={showTopMenu} />

        <div className="flex min-w-0 grow flex-col">
          <main className={className || 'grow content relative content-start'}>
            {children}
          </main>
          {showTopMenu && (
            <>
              {!hideCta && <CallToAction url={url} />}
              <Footer />
            </>
          )}
        </div>
      </div>
    </>
  );
});
