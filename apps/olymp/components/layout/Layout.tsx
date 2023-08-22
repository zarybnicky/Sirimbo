import { ErrorPage } from '@app/ui/ErrorPage';
import { LoginForm } from '@app/ui/LoginForm';
import { useAuth } from '@app/ui/use-auth';
import { DefaultSeo } from 'next-seo';
import dynamic from 'next/dynamic';
import { useRouter } from 'next/router';
import React from 'react';
import Footer from './Footer';
import { Header } from './Header';
import { Sidebar } from './Sidebar';
import { CallToAction } from 'components/CallToAction';
const FeedbackForm = dynamic(() => import('@app/ui/FeedbackForm'), { ssr: false });
import { tenantConfig } from '@app/tenant/config.mjs';
import { cn } from '@app/ui/cn';

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
      <DefaultSeo
        titleTemplate={`%s · ${tenantConfig.shortName}`}
        defaultTitle={tenantConfig.shortName}
        themeColor="#000"
        facebook={{ appId: '704526480597551' }}
        openGraph={{ siteName: tenantConfig.shortName }}
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

        <div className={cn("grow content relative content-start", className)}>
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
