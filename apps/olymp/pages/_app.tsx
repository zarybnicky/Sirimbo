import * as React from 'react';
import { AppProps, NextWebVitalsMetric } from 'next/app';
import Router from 'next/router';
import { event } from 'nextjs-google-analytics';
import { ProvideAuth } from '@app/ui/use-auth';
import { Layout, type LayoutProps } from 'components/layout/Layout';
import { Tracking } from '@app/ui/Tracking';
import { ConfirmProvider } from '@app/ui/Confirm';
import { NextPage } from 'next';
import { DefaultSeo } from 'next-seo';
import i18next from 'i18next';
import { z } from 'zod';
import { makeZodI18nMap } from 'zod-i18n-map';
import csZodTranslation from 'public/locales/cs/zod.json';
import { configureUrql } from '@app/graphql/query';
import { withUrqlClient } from 'next-urql';
import NProgress from 'nprogress';
import { ToastContainer } from 'react-toastify';

import 'glider-js/glider.min.css';
import 'public/style/index.css';
import 'nprogress/nprogress.css';
import 'react-toastify/dist/ReactToastify.css';

Router.events.on('routeChangeStart', () => NProgress.start());
Router.events.on('routeChangeComplete', () => NProgress.done());
Router.events.on('routeChangeError', () => NProgress.done());

void i18next.init({
  lng: 'cs',
  resources: {
    cs: { zod: csZodTranslation },
  },
});
z.setErrorMap(makeZodI18nMap({
  ns: 'zod',
  t: i18next.t,
}));

export type NextPageWithLayout<P = object, IP = P> = NextPage<P, IP> &
  Omit<LayoutProps, 'children'>;

type AppPropsWithLayout<T = object> = AppProps<T> & {
  Component: NextPageWithLayout<T>;
  resetUrqlClient: () => void;
};

function App({ Component, pageProps, resetUrqlClient }: AppPropsWithLayout) {
  const { staticTitle, isDetail, list, showTopMenu, hideTopMenuIfLoggedIn, permissions, requireLoggedOut } = Component;
  const layoutProps = { staticTitle, isDetail, list, showTopMenu, hideTopMenuIfLoggedIn, permissions, requireLoggedOut };
  return (
    <ProvideAuth onReset={resetUrqlClient}>
      <ConfirmProvider>
        <Tracking />
        <DefaultSeo
          titleTemplate="%s · TK Olymp"
          defaultTitle="TK Olymp"
          themeColor="#000"
          facebook={{ appId: '704526480597551' }}
          openGraph={{
            siteName: 'TK Olymp',
          }}
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
        <Layout {...layoutProps} >
          <Component {...pageProps} />
        </Layout>
        <ToastContainer limit={3} />
      </ConfirmProvider>
    </ProvideAuth>
  );
}

export default withUrqlClient(configureUrql, { ssr: false })(App);

export function reportWebVitals({ id, name, label, value }: NextWebVitalsMetric) {
  if (label === 'web-vital') {
    event(name, {
      category: 'Web Vitals',
      value: Math.round(name === 'CLS' ? value * 1000 : value), // values must be integers
      label: id, // id unique to current page load
      nonInteraction: true, // avoids affecting bounce rate.
    });
  }
}
