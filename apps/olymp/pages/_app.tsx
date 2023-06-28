import * as React from 'react';
import { AppProps, NextWebVitalsMetric } from 'next/app';
import Router from 'next/router';
import { event } from 'nextjs-google-analytics';
import { ProvideAuth } from '@app/ui/use-auth';
import { Layout, type LayoutProps } from 'components/layout/Layout';
import 'public/style/index.css';
import { Tracking } from '@app/ui/Tracking';
import { ConfirmProvider } from '@app/ui/Confirm';
import { NextPage } from 'next';
import { DefaultSeo } from 'next-seo';
import i18next from 'i18next';
import { z } from 'zod';
import { zodI18nMap } from 'zod-i18n-map';
import csZodTranslation from 'public/locales/cs/zod.json';
import { withPreconfiguredUrql } from '@app/graphql/query';

import 'nprogress/nprogress.css';
import NProgress from 'nprogress';

import 'react-toastify/dist/ReactToastify.css';
import { ToastContainer } from 'react-toastify';

Router.events.on('routeChangeStart', () => NProgress.start());
Router.events.on('routeChangeComplete', () => NProgress.done());
Router.events.on('routeChangeError', () => NProgress.done());

void i18next.init({
  lng: 'cs',
  resources: {
    cs: { zod: csZodTranslation },
  },
});
z.setErrorMap(zodI18nMap);

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

export default withPreconfiguredUrql(App);

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
