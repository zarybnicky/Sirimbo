import { tenantConfig } from '@/tenant/config.js';
import { TenantSeo } from '@/tenant/current/ui';
import { ErrorPage } from '@/ui/ErrorPage';
import { LoginForm } from '@/ui/LoginForm';
import { useAuth } from '@/ui/use-auth';
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

export function Layout({
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
  const { user, isLoading, perms } = useAuth();

  showTopMenu = showTopMenu && tenantConfig.enableHome;
  if (hideTopMenuIfLoggedIn) {
    showTopMenu = !user;
  }

  const missingPermission =
    (requireUser && !perms.isLoggedIn) ||
    (requireMember && (!perms.isMember && !perms.isTrainer && !perms.isTrainer)) ||
    (requireTrainer && !perms.isTrainerOrAdmin) ||
    (requireAdmin && !perms.isAdmin);
  if (!isLoading && missingPermission) {
    children = !!user ? (
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
}
