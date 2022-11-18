import * as React from "react";
import { AppProps, NextWebVitalsMetric } from 'next/app';
import Router from "next/router";
import NProgress from "nprogress";
import { event } from "nextjs-google-analytics";
import { ProvideAuth } from 'lib/data/use-auth';
import { Hydrate, QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { persistQueryClient } from '@tanstack/react-query-persist-client';
import { createSyncStoragePersister } from '@tanstack/query-sync-storage-persister';
import { Layout } from "components/layout/Layout";
import { ProvideMeta } from "lib/use-meta";
import 'public/style/index.css';
import { Tracking } from "components/Tracking";
import { ToastContainer } from 'react-toastify';
import { ConfirmProvider } from 'components/Confirm';
import { NextPage } from "next";
import dynamic from "next/dynamic";

Router.events.on("routeChangeStart", () => NProgress.start());
Router.events.on("routeChangeComplete", () => NProgress.done());
Router.events.on("routeChangeError", () => NProgress.done());

const ReactQueryDevtools = dynamic(
  () => import('@tanstack/react-query-devtools').then(x => x.ReactQueryDevtools),
  { ssr: false },
);

const WithProviders = (children: React.ReactNode, pageProps: AppProps['pageProps']) => {
  const queryClientRef = React.useRef<QueryClient>()
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
      })
    };
  }

  return (
    <QueryClientProvider client={queryClientRef.current}>
      {process.env.NODE_ENV === "development" && <ReactQueryDevtools initialIsOpen={false} />}
      <Hydrate state={pageProps.dehydratedState}>
        <ProvideAuth>
          <ProvideMeta>
            <ConfirmProvider>
              <Tracking />
              {children}
              <ToastContainer limit={3} />
            </ConfirmProvider>
          </ProvideMeta>
        </ProvideAuth>
      </Hydrate>
    </QueryClientProvider>
  );
};

export type NextPageWithLayout<P = {}, IP = P> = NextPage<P, IP> & {
  getLayout?: (page: React.ReactElement) => React.ReactNode
}

type AppPropsWithLayout = AppProps & {
  Component: NextPageWithLayout
}

const defaultLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;

export default function App({ Component, pageProps }: AppPropsWithLayout) {
  const getLayout = Component.getLayout ?? defaultLayout;
  return WithProviders(getLayout(<Component {...pageProps} />), pageProps);
}

export function reportWebVitals({ id, name, label, value }: NextWebVitalsMetric) {
  if (label === "web-vital") {
    event(name, {
      category: "Web Vitals",
      value: Math.round(name === "CLS" ? value * 1000 : value), // values must be integers
      label: id, // id unique to current page load
      nonInteraction: true, // avoids affecting bounce rate.
    });
  }
}
