import classNames from 'classnames';
import { ErrorPage } from 'components/ErrorPage';
import { useAuth } from 'lib/data/use-auth';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { NextSeo } from 'next-seo';
import { useRouter } from 'next/router';
import React from 'react';
import { Footer } from './Footer';
import { Header } from './Header';
import { Sidebar } from './Sidebar';
import dynamic from 'next/dynamic';
const FeedbackForm = dynamic(() => import('components/FeedbackForm'), { ssr: false });

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

  React.useEffect(() => {
    const scroll = () =>
      document.querySelector('[data-nextjs-scroll-focus-boundary]')?.scrollTo(0, 0);
    router.events.on('routeChangeComplete', scroll);
    return () => router.events.off('routeChangeComplete', scroll);
  }, [router]);

  if (hideTopMenuIfLoggedIn) {
    showTopMenu = !user;
  }

  if (!isLoading && user && requireLoggedOut) {
    router.replace('/dashboard');
  }
  if (!isLoading && permissions) {
    if (!perms.hasPermission(permissions[0], permissions[1])) {
      if (!perms.userId) {
        router.replace({ pathname: '/login', query: { from: router.asPath } });
        children = null;
      } else {
        children = (
          <ErrorPage
            error="Přístup zamítnut"
            details="Nemáte dostatečná práva pro zobrazení této stránky"
          />
        );
      }
    }
  }

  return (
    <div className="h-screen flex flex-col w-full relative overflow-hidden">
      {staticTitle && <NextSeo title={staticTitle} />}
      <Header {...{ isOpen, setIsOpen, showTopMenu }} />
      <div className="relative grow flex overflow-hidden bg-stone-100">
        <Sidebar {...{ isOpen, setIsOpen, showTopMenu }} />
        {list && (
          <div
            className={classNames(
              'grow',
              isDetail
                ? 'hidden lg:flex lg:grow-0 flex-col'
                : 'max-h-screen min-h-screen w-full',
            )}
          >
            {list}
          </div>
        )}
        <div
          data-nextjs-scroll-focus-boundary
          className={classNames(
            list && isDetail && 'grow content min-h-0 overflow-y-auto',
            list && !isDetail && 'hidden lg:flex',
            !list && 'relative h-full grow overflow-y-auto content',
            'scrollbar',
          )}
        >
          {children}
          {showTopMenu && <Footer />}
        </div>
        <FeedbackForm />
      </div>
    </div>
  );
}
