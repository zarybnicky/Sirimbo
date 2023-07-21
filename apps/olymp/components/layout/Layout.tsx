import { ErrorPage } from '@app/ui/ErrorPage';
import { LoginForm } from '@app/ui/LoginForm';
import { useAuth } from '@app/ui/use-auth';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import classNames from 'classnames';
import { NextSeo } from 'next-seo';
import dynamic from 'next/dynamic';
import { useRouter } from 'next/router';
import React from 'react';
import Footer from './Footer';
import { Header } from './Header';
import { Sidebar } from './Sidebar';
const FeedbackForm = dynamic(() => import('@app/ui/FeedbackForm'), { ssr: false });

export type LayoutProps = {
  hideTopMenuIfLoggedIn?: boolean;
  showTopMenu?: boolean;
  list?: React.ReactNode;
  isDetail?: boolean;
  permissions?: [PermissionKey, PermissionLevel];
  requireLoggedOut?: boolean;
  staticTitle?: string;
  children?: React.ReactNode;
};

export function Layout({
  children,
  showTopMenu,
  hideTopMenuIfLoggedIn,
  list,
  isDetail,
  permissions,
  staticTitle,
  requireLoggedOut,
}: LayoutProps) {
  const [isOpen, setIsOpen] = React.useState(false);
  const router = useRouter();
  const { user, isLoading, perms } = useAuth();

  showTopMenu = showTopMenu && !!process.env.NEXT_PUBLIC_ENABLE_HOME;
  if (hideTopMenuIfLoggedIn) {
    showTopMenu = !user;
  }

  if (!isLoading && user && requireLoggedOut) {
    void router.replace('/dashboard');
  }
  if (!isLoading && permissions && !perms.hasPermission(permissions[0], permissions[1])) {
    children = perms.userId ? (
      <ErrorPage
        error="Přístup zamítnut"
        details="Nemáte dostatečná práva pro zobrazení této stránky"
      />
    ) : (
      <LoginForm />
    );
  }

  return (
    <div className="h-screen flex flex-col w-full relative overflow-hidden">
      {staticTitle && <NextSeo title={staticTitle} />}
      <Header {...{ isOpen, setIsOpen, showTopMenu }} />

      <div className="relative grow flex overflow-hidden bg-neutral-1 text-accent-12">
        <Sidebar {...{ isOpen, setIsOpen, showTopMenu }} />

        {list && (
          <div
            className={classNames(
              'grow flex-none lg:w-80 xl:w-96',
              'border-r lg:border-accent-6 lg:bg-accent-3 dark:lg:bg-accent-4',
              isDetail
                ? 'hidden lg:flex lg:grow-0 flex-col'
                : 'max-h-screen min-h-screen w-full',
            )}
          >
            {list}
          </div>
        )}

        <div className={classNames('scrollbar overflow-y-auto grow content relative', {
          '': !list,
          'hidden lg:grid': list && !isDetail,
          'min-h-0': list && isDetail,
        })}>
          {children}
          {showTopMenu && <Footer />}
        </div>

        <FeedbackForm />
      </div>
    </div>
  );
}
