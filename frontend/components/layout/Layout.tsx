import classNames from 'classnames';
import { useAuth } from 'lib/data/use-auth';
import { useRouter } from 'next/router';
import React from 'react';
import { Footer } from './Footer';
import { Header } from './Header';
import { Sidebar } from './Sidebar';

export type LayoutProps = {
  hideTopMenuIfLoggedIn?: boolean;
  showTopMenu?: boolean;
  list?: React.ReactNode;
  isDetail?: boolean;
  children?: React.ReactNode;
};

export function Layout({ children, showTopMenu, hideTopMenuIfLoggedIn, list, isDetail }: LayoutProps) {
  const [isOpen, setIsOpen] = React.useState(false);
  const router = useRouter();
  const { user } = useAuth();
  if (hideTopMenuIfLoggedIn) {
    showTopMenu = !!user;
  }

  React.useEffect(() => {
    const scroll = () =>
      document.querySelector('[data-nextjs-scroll-focus-boundary]')?.scrollTo(0, 0);
    router.events.on('routeChangeComplete', scroll);
    return () => router.events.off('routeChangeComplete', scroll);
  }, [router]);

  return (
    <div className="h-screen flex flex-col w-full relative overflow-hidden">
      <Header {...{ isOpen, setIsOpen, showTopMenu }} />
      <div className="relative grow flex overflow-hidden">
        <Sidebar {...{ isOpen, setIsOpen, showTopMenu }} />
        {list ? (
          <>
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
            <div
              data-nextjs-scroll-focus-boundary
              className={classNames(
                isDetail ? 'grow content min-h-0 overflow-y-auto' : 'hidden lg:flex',
                'scrollbar',
              )}
            >
              {children}
            </div>
          </>
        ) : (
          <div
            data-nextjs-scroll-focus-boundary
            className="scrollbar relative h-full grow overflow-y-auto content"
          >
            {children}
            {showTopMenu && <Footer />}
          </div>
        )}
      </div>
    </div>
  );
}
