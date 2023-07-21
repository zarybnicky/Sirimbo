import { ErrorPage } from '@app/ui/ErrorPage';
import { LoginForm } from '@app/ui/LoginForm';
import { useAuth } from '@app/ui/use-auth';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { DefaultSeo } from 'next-seo';
import dynamic from 'next/dynamic';
import { useRouter } from 'next/router';
import React from 'react';
import Footer from './Footer';
import { Header } from './Header';
import { Sidebar } from './Sidebar';
import { CallToAction } from 'components/CallToAction';
const FeedbackForm = dynamic(() => import('@app/ui/FeedbackForm'), { ssr: false });

export type LayoutProps = {
  hideTopMenuIfLoggedIn?: boolean;
  showTopMenu?: boolean;
  permissions?: [PermissionKey, PermissionLevel];
  children?: React.ReactNode;
};

export function Layout({
  children,
  showTopMenu,
  hideTopMenuIfLoggedIn,
  permissions,
}: LayoutProps) {
  const router = useRouter();
  const [isOpen, setIsOpen] = React.useState(false);
  const { user, isLoading, perms } = useAuth();

  showTopMenu = showTopMenu && !!process.env.NEXT_PUBLIC_ENABLE_HOME;
  if (hideTopMenuIfLoggedIn) {
    showTopMenu = !user;
  }

  if (!isLoading && permissions && !perms.hasPermission(permissions[0], permissions[1])) {
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
      <DefaultSeo
        titleTemplate="%s · TK Olymp"
        defaultTitle="TK Olymp"
        themeColor="#000"
        facebook={{ appId: '704526480597551' }}
        openGraph={{ siteName: 'TK Olymp' }}
        additionalMetaTags={[
          { name: "wot-verification", content: "ec0cf41ab42dae52d3d4" },
          { name: "msvalidate.01", content: "7BD6C8B5748FC22EF06AB3AE89900885" },
          { name: "facebook-domain-verification", content: "k8tt64a93roxiymxo79clpvklan9j2" },
          { name: "google-site-verification", content: "Hfe7zlgTDOIpJv4rKGQz2Xg8Aezb6sIO0aAxVhrml9w" },
          { name: "norton-safeweb-site-verification", content: "r44xj2vskhlgkyqcqm1hdgga2jdfj-idvyys0277y96s72k-tq0z-yyjdu7h3el6pi2gek0i4ykq3xgiguufrvuhj8nbj4n4miwjhvumhp35jfrafyynhj4ee8ctzpzh" },
        ]}
        additionalLinkTags={[
          {
            rel: 'apple-touch-icon',
            sizes: '180x180',
            href: '/apple-touch-icon.png?v=3',
          },
          { rel: 'icon', sizes: '32x32', href: '/favicon-32x32.png?v=3' },
          { rel: 'icon', sizes: '16x16', href: '/favicon-16x16.png?v=3' },
          { rel: 'shortcut icon', href: '/favicon.ico?v=3' },
          { rel: 'manifest', href: '/site.webmanifest?v=3' },
          {
            rel: 'mask-icon',
            color: '#5bbad5',
            href: '/safari-pinned-tab.svg?v=3',
          },
        ]}
      />
      <Header {...{ isOpen, setIsOpen, showTopMenu }} />

      <div className="relative flex bg-neutral-1 text-accent-12">
        <Sidebar {...{ isOpen, setIsOpen, showTopMenu }} />

        <div className='grow content relative'>
          {children}
          {showTopMenu && (
            <>
              <CallToAction url={router.asPath} />
              <Footer />
            </>
          )}
        </div>
      </div>

      <FeedbackForm />
    </>
  );
}
