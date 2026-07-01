'use client';

import { CallToAction } from '@/ui/CallToAction';
import { ErrorPage } from '@/ui/ErrorPage';
import { LoginForm } from '@/ui/forms/LoginForm';
import { Header } from '@/ui/Header.app';
import { Sidebar } from '@/ui/Sidebar.app';
import { tenantConfigAtom } from '@/ui/state/auth';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { useAtomValue } from 'jotai';
import { usePathname, useSearchParams } from 'next/navigation';
import React from 'react';

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
  desktopLogo: React.ReactNode;
  mobileLogo: React.ReactNode;
  sidebarLogo: React.ReactNode;
  socialIcons: React.ReactNode;
  footer: React.ReactNode;
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
}: LayoutProps) {
  const [isOpen, setIsOpen] = React.useState(false);
  const pathname = usePathname() || '/';
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const { enableHome } = useAtomValue(tenantConfigAtom);

  showTopMenu = showTopMenu && enableHome;
  if (hideTopMenuIfLoggedIn) {
    showTopMenu = !auth.user;
  }
  const shouldShowTopMenu = !!showTopMenu;

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
      <Header
        isOpen={isOpen}
        setIsOpen={setIsOpen}
        showTopMenu={shouldShowTopMenu}
        desktopLogo={desktopLogo}
        mobileLogo={mobileLogo}
        socialIcons={socialIcons}
      />

      <div className="flex min-h-[calc(100dvh-52px)] md:min-h-[calc(100dvh-68px)]">
        <Sidebar
          isOpen={isOpen}
          setIsOpen={setIsOpen}
          showTopMenu={shouldShowTopMenu}
          sidebarLogo={sidebarLogo}
        />

        <div className={className || 'grow content relative content-start'}>
          {children}
          {shouldShowTopMenu && (
            <>
              {!hideCta && <CallToAction url={pathname} />}
              {footer}
            </>
          )}
        </div>
      </div>
    </>
  );
});
