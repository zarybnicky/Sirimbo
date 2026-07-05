'use client';

import { getTenantUi } from '@/tenant/catalog';
import { ErrorPage } from '@/ui/ErrorPage';
import { LoginForm } from '@/ui/forms/LoginForm';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { CallToAction } from '@/ui/CallToAction';
import React, { useMemo } from 'react';
import { Header } from '@/ui/Header';
import { Sidebar } from '@/ui/Sidebar';
import { tenantConfigAtom, tenantIdAtom } from './state/auth';
import { useAtomValue } from 'jotai';
import { TenantSeo } from '@/tenant/TenantSeo';
import { usePathname, useSearchParams } from 'next/navigation';

type LayoutProps = {
  hideTopMenuIfLoggedIn?: boolean;
  showTopMenu?: boolean;
  children?: React.ReactNode;
  hideCta?: boolean;
  requireUser?: boolean;
  requireMember?: boolean;
  requireAdmin?: boolean;
  requireTrainer?: boolean;
  requireSystemAdmin?: boolean;
  className?: string;
  desktopLogo?: React.ReactNode;
  mobileLogo?: React.ReactNode;
  sidebarLogo?: React.ReactNode;
  socialIcons?: React.ReactNode;
  footer?: React.ReactNode;
  includeTenantSeo?: boolean;
};

export const Layout = React.memo(function Layout({
  children,
  showTopMenu,
  hideTopMenuIfLoggedIn,
  hideCta,
  requireUser,
  requireMember,
  requireAdmin,
  requireTrainer,
  requireSystemAdmin,
  className,
  desktopLogo,
  mobileLogo,
  sidebarLogo,
  socialIcons,
  footer,
  includeTenantSeo = true,
}: LayoutProps) {
  const pathname = usePathname();
  const searchParams = useSearchParams();
  const [isOpen, setIsOpen] = React.useState(false);
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const tenantId = useAtomValue(tenantIdAtom);
  const { enableHome } = useAtomValue(tenantConfigAtom);
  const Footer = useMemo(() => getTenantUi(tenantId, 'Footer'), [tenantId]);
  const search = searchParams?.toString() ?? '';
  const currentUrl = search ? `${pathname}?${search}` : pathname;

  showTopMenu = showTopMenu && enableHome;
  if (hideTopMenuIfLoggedIn) {
    showTopMenu = !auth.user;
  }

  const missingPermission =
    (requireUser && !auth.isLoggedIn) ||
    (requireMember && !auth.isMember && !auth.isTrainerOrAdmin) ||
    (requireTrainer && !auth.isTrainerOrAdmin) ||
    (requireAdmin && !auth.isAdmin) ||
    (requireSystemAdmin && !auth.isSystemAdmin);
  if (!authLoading && missingPermission) {
    children = auth.user ? (
      <ErrorPage
        error="Přístup zamítnut"
        details="Nemáte dostatečná práva pro zobrazení této stránky"
      />
    ) : (
      <LoginForm />
    );
  }

  return (
    <>
      {includeTenantSeo && <TenantSeo tenantId={tenantId} />}
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

        <div className={className || 'grow content relative content-start'}>
          {children}
          {showTopMenu && (
            <>
              {!hideCta && <CallToAction url={currentUrl} />}
              {footer ?? <Footer />}
            </>
          )}
        </div>
      </div>
    </>
  );
});
