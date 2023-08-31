import { tenantConfig } from '@app/tenant/config.js';
import { TenantSeo } from '@app/tenant/current/ui';
import { ErrorPage } from '@app/ui/ErrorPage';
import { LoginForm } from '@app/ui/LoginForm';
import { cn } from '@app/ui/cn';
import { useAuth } from '@app/ui/use-auth';
import { CallToAction } from '@/components/CallToAction';
import dynamic from 'next/dynamic';
import { useRouter } from 'next/router';
import React from 'react';
import Footer from './Footer';
import { Header } from './Header';
import { Sidebar } from './Sidebar';
const FeedbackForm = dynamic(() => import('@app/ui/FeedbackForm'), { ssr: false });

type LayoutProps = {
  hideTopMenuIfLoggedIn?: boolean;
  showTopMenu?: boolean;
  children?: React.ReactNode;
  hideCta?: boolean;
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
    (requireMember && !perms.isTenantMember) ||
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

      <div className="relative flex bg-neutral-1 text-accent-12 min-h-[calc(100%-48px)] md:min-h-[calc(100%-64px)]">
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

      {tenantConfig.enableHome && <FeedbackForm />}
    </>
  );
}
