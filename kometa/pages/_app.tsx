import * as React from 'react';
import { AppProps, NextWebVitalsMetric } from 'next/app';
import { ProvideAuth } from 'lib/use-auth';
import { Layout, type LayoutProps } from 'components/Layout';
import 'public/style/index.css';
import { NextPage } from 'next';
import { DefaultSeo } from 'next-seo';
import { withPreconfiguredUrql } from '@app/graphql/query';
import { ToastProvider, ToastViewport } from '@app/ui/toast';

export type NextPageWithLayout<P = {}, IP = P> = NextPage<P, IP> &
  Omit<LayoutProps, 'children'>;

type AppPropsWithLayout<T = {}> = AppProps<T> & {
  Component: NextPageWithLayout<T>;
};

function App({ Component, pageProps }: AppPropsWithLayout) {
  const { isDetail, list } = Component;
  const layoutProps = { isDetail, list };
  return (
    <ProvideAuth>
      <ToastProvider swipeDirection="right">
        <DefaultSeo
          titleTemplate="%s · Rozpisovník"
          defaultTitle="Rozpisovník"
          themeColor="#000"
          openGraph={{
            siteName: 'Rozpisovník',
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
        <ToastViewport />
      </ToastProvider>
    </ProvideAuth>
  );
}

export default withPreconfiguredUrql(App);
