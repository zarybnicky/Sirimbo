import * as React from 'react';
import { AppProps } from 'next/app';
import { ProvideAuth } from '@app/ui/use-auth';
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
  resetUrqlClient: () => void;
};

function App({ Component, pageProps, resetUrqlClient }: AppPropsWithLayout) {
  const { isDetail, list } = Component;
  const layoutProps = { isDetail, list };
  return (
    <ProvideAuth onReset={resetUrqlClient}>
      <ToastProvider swipeDirection="right">
        <DefaultSeo
          titleTemplate="%s · Rozpisovník"
          defaultTitle="Rozpisovník"
          themeColor="#000"
          openGraph={{
            siteName: 'Rozpisovník',
          }}
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
