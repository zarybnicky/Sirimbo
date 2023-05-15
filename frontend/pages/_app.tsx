import * as React from 'react';
import { AppProps, NextWebVitalsMetric } from 'next/app';
import Router from 'next/router';
import NProgress from 'nprogress';
import { event } from 'nextjs-google-analytics';
import { ProvideAuth } from 'lib/data/use-auth';
import { Hydrate, QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { persistQueryClient } from '@tanstack/react-query-persist-client';
import { createSyncStoragePersister } from '@tanstack/query-sync-storage-persister';
import { Layout, type LayoutProps } from 'components/layout/Layout';
import 'public/style/index.css';
import { Tracking } from 'components/Tracking';
import { ToastContainer } from 'react-toastify';
import { ConfirmProvider } from 'components/Confirm';
import { NextPage } from 'next';
import dynamic from 'next/dynamic';
import { DefaultSeo } from 'next-seo';

Router.events.on('routeChangeStart', () => NProgress.start());
Router.events.on('routeChangeComplete', () => NProgress.done());
Router.events.on('routeChangeError', () => NProgress.done());

const ReactQueryDevtools = dynamic(
  () => import('@tanstack/react-query-devtools').then((x) => x.ReactQueryDevtools),
  { ssr: false },
);

const WithProviders = <T extends { dehydratedState?: object }>(
  children: React.ReactNode,
  pageProps: T,
) => {
  const queryClientRef = React.useRef<QueryClient>();
  if (!queryClientRef.current) {
    queryClientRef.current = new QueryClient({
      defaultOptions: {
        queries: {
          refetchOnWindowFocus: false,
          cacheTime: 1000 * 60 * 60 * 24, // 24 hours
        },
      },
    });

    if (typeof window !== 'undefined') {
      persistQueryClient({
        queryClient: queryClientRef.current,
        persister: createSyncStoragePersister({
          storage: window.localStorage,
          key: 'REACT_QUERY_OFFLINE_CACHE_1.0',
        }),
      });
    }
  }

  return (
    <QueryClientProvider client={queryClientRef.current}>
      {process.env.NODE_ENV === 'development' && (
        <ReactQueryDevtools initialIsOpen={false} />
      )}
      <Hydrate state={pageProps.dehydratedState}>
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
      </Hydrate>
    </QueryClientProvider>
  );
};

export type NextPageWithLayout<P = {}, IP = P> = NextPage<P, IP> & Omit<LayoutProps, 'children'>;

type AppPropsWithLayout<
  T extends { dehydratedState?: object } = { dehydratedState?: object },
> = AppProps<T> & {
  Component: NextPageWithLayout<T>;
};

export default function App({ Component, pageProps }: AppPropsWithLayout) {
  const { isDetail, list, showTopMenu, hideTopMenuIfLoggedIn, permissions, staticTitle, requireLoggedOut } = Component;
  return WithProviders(
    <Layout {...{ isDetail, list, showTopMenu, hideTopMenuIfLoggedIn, permissions, staticTitle, requireLoggedOut }}>
      <Component {...pageProps} />
    </Layout>,
    pageProps,
  );
}

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
