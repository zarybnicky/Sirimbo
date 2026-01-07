import { getTenantUi } from '@/tenant/catalog';
import { ErrorPage } from '@/ui/ErrorPage';
import { LoginForm } from '@/ui/forms/LoginForm';
import { useAuth, useAuthLoading } from '@/ui/use-auth';
import { CallToAction } from '@/ui/CallToAction';
import { useRouter } from 'next/router';
import React, { useMemo } from 'react';
import { Header } from '@/ui/Header';
import { Sidebar } from '@/ui/Sidebar';
import { tenantConfigAtom, tenantIdAtom } from './state/auth';
import { useAtomValue } from 'jotai';

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
}: LayoutProps) {
  const router = useRouter();
  const [isOpen, setIsOpen] = React.useState(false);
  const auth = useAuth();
  const authLoading = useAuthLoading();
  const tenantId = useAtomValue(tenantIdAtom);
  const { enableHome } = useAtomValue(tenantConfigAtom);
  const TenantSeo = useMemo(() => getTenantUi(tenantId, 'TenantSeo'), [tenantId]);
  const Footer = useMemo(() => getTenantUi(tenantId, 'Footer'), [tenantId]);

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
      <TenantSeo />
      <Header {...{ isOpen, setIsOpen, showTopMenu }} />

      <div className="flex min-h-[calc(100dvh-52px)] md:min-h-[calc(100dvh-68px)]">
        <Sidebar {...{ isOpen, setIsOpen, showTopMenu }} />

        <div className={className || 'grow content relative content-start'}>
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
