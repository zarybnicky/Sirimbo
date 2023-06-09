import * as React from 'react';
import { AppProps, NextWebVitalsMetric } from 'next/app';
import Router from 'next/router';
import NProgress from 'nprogress';
import { event } from 'nextjs-google-analytics';
import { ProvideAuth } from 'lib/data/use-auth';
import { Layout, type LayoutProps } from 'components/layout/Layout';
import 'public/style/index.css';
import { Tracking } from 'components/Tracking';
import { ToastContainer } from 'react-toastify';
import { ConfirmProvider } from 'components/Confirm';
import { NextPage } from 'next';
import { DefaultSeo } from 'next-seo';
import i18next from 'i18next';
import { z } from 'zod';
import { zodI18nMap } from 'zod-i18n-map';
import csZodTranslation from 'public/locales/cs/zod.json';
import { withUrqlClient } from 'next-urql';
import { makeDefaultStorage } from '@urql/exchange-graphcache/default-storage';
import { fetchExchange } from 'urql';
import { offlineExchange } from '@urql/exchange-graphcache';
import { retryExchange } from '@urql/exchange-retry';
import { refocusExchange } from '@urql/exchange-refocus';
import { requestPolicyExchange } from '@urql/exchange-request-policy';
import { origin, cacheConfig } from 'lib/query';
import schema from 'lib/graphql/introspection.json';
import { devtoolsExchange } from '@urql/devtools';

Router.events.on('routeChangeStart', () => NProgress.start());
Router.events.on('routeChangeComplete', () => NProgress.done());
Router.events.on('routeChangeError', () => NProgress.done());

i18next.init({
  lng: 'cs',
  resources: {
    cs: { zod: csZodTranslation },
  },
});
z.setErrorMap(zodI18nMap);

const WithProviders = (children: React.ReactNode) => {
  return (
    <ProvideAuth>
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
        {children}
        <ToastContainer limit={3} />
      </ConfirmProvider>
    </ProvideAuth>
  );
};

export type NextPageWithLayout<P = {}, IP = P> = NextPage<P, IP> &
  Omit<LayoutProps, 'children'>;

type AppPropsWithLayout<T = {}> = AppProps<T> & {
  Component: NextPageWithLayout<T>;
};

function App({ Component, pageProps }: AppPropsWithLayout) {
  const {
    isDetail,
    list,
    showTopMenu,
    hideTopMenuIfLoggedIn,
    permissions,
    staticTitle,
    requireLoggedOut,
  } = Component;
  return WithProviders(
    <Layout
      {...{
        isDetail,
        list,
        showTopMenu,
        hideTopMenuIfLoggedIn,
        permissions,
        staticTitle,
        requireLoggedOut,
      }}
    >
      <Component {...pageProps} />
    </Layout>,
  );
}

export default withUrqlClient(
  (ssrExchange) => ({
    url: `${origin}/graphql`,
    exchanges: [
      process.env.NODE_ENV !== 'production' ? devtoolsExchange : (({forward}) => forward),
      refocusExchange(),
      requestPolicyExchange({ttl: 60 * 1000}),
      typeof window !== 'undefined' ? offlineExchange({
        schema,
        storage: makeDefaultStorage({
          idbName: 'graphcache-v3',
          maxAge: 7,
        }),
        ...cacheConfig,
      }) : (({forward}) => forward),
      retryExchange({
        initialDelayMs: 1000,
        maxDelayMs: 15000,
        randomDelay: true,
        maxNumberAttempts: 2,
        retryIf: (err) => !!err && !!err.networkError,
      }),
      ssrExchange,
      fetchExchange,
    ],
    fetchOptions: {
      credentials: 'include',
      headers: {
        ...(process.env.NEXT_PUBLIC_TENANT_ID ? {
          'x-tenant-id': process.env.NEXT_PUBLIC_TENANT_ID,
        } : {}),
      },
    },
  }),
  { ssr: false },
)(App);

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
