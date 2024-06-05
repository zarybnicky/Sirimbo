import { tenantConfig } from '@/tenant/config.js';
import { TenantSeo } from '@/tenant/current/ui';
import { ErrorPage } from '@/ui/ErrorPage';
import { LoginForm } from '@/ui/forms/LoginForm';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { CallToAction } from '@/components/CallToAction';
import { useRouter } from 'next/router';
import React from 'react';
import Footer from './Footer';
import { Header } from './Header';
import { Sidebar } from './Sidebar';

type LayoutProps = {
  hideTopMenuIfLoggedIn?: boolean;
  showTopMenu?: boolean;
  children?: React.ReactNode;
  hideCta?: boolean;
  requireUser?: boolean;
  requireMember?: boolean;
  requireAdmin?: boolean;
  requireTrainer?: boolean;
  className?: string;
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
  className,
}: LayoutProps) {
  const router = useRouter();
  const [isOpen, setIsOpen] = React.useState(false);
  const auth = useAuth();
  const authLoading = useAuthLoading();

  showTopMenu = showTopMenu && tenantConfig.enableHome;
  if (hideTopMenuIfLoggedIn) {
    showTopMenu = !auth.user;
  }

  const missingPermission =
    (requireUser && !auth.isLoggedIn) ||
    (requireMember && !auth.isMember && !auth.isTrainerOrAdmin) ||
    (requireTrainer && !auth.isTrainerOrAdmin) ||
    (requireAdmin && !auth.isAdmin);
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
      <TenantSeo />
      <Header {...{ isOpen, setIsOpen, showTopMenu }} />

      <div className="flex min-h-[calc(100dvh-52px)] md:min-h-[calc(100dvh-68px)]">
        <Sidebar {...{ isOpen, setIsOpen, showTopMenu }} />

        <div className={className || "grow content relative content-start"}>
          {children}
          {showTopMenu && (
            <>
              {!hideCta && <CallToAction url={router.asPath} />}
              <Footer />
            </>
          )}
        </div>
      </div>
    </>
  );
});
