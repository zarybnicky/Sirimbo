import * as React from "react";
import { AppProps, NextWebVitalsMetric } from 'next/app';
import Router from "next/router";
import NProgress from "nprogress";
import posthog from "posthog-js";
import { event } from "nextjs-google-analytics";
import { ProvideAuth } from 'lib/data/use-auth';
import { ThemeProvider } from "@mui/material";
import { theme } from "lib/theme";
import { SnackbarProvider } from "notistack";
import { LocalizationProvider } from "@mui/x-date-pickers/LocalizationProvider";
import { AdapterDateFns } from '@mui/x-date-pickers/AdapterDateFns';
import { Hydrate, QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { persistQueryClient } from '@tanstack/react-query-persist-client';
import { createSyncStoragePersister } from '@tanstack/query-sync-storage-persister';
import { ConfirmProvider } from 'material-ui-confirm';
import { Layout } from "components/Layout";
import { ProvideMeta } from "lib/use-meta";
import "nprogress/nprogress.css";
import 'public/style/material-icons.css';
import '@react-page/editor/lib/index.css';
import '@react-page/plugins-slate/lib/index.css';
import '@react-page/plugins-image/lib/index.css';

Router.events.on("routeChangeStart", () => NProgress.start());
Router.events.on("routeChangeComplete", () => NProgress.done());
Router.events.on("routeChangeError", () => NProgress.done());


export default function App({ Component, pageProps }: AppProps) {
  React.useEffect(() => {
    if (process.env.NODE_ENV !== 'development') {
      posthog.init('phc_H2WM9q2xXVFl1wEak9TcQVAsOpWNGuauzAvyOmBquYQ', {
        api_host: 'https://eu.posthog.com',
      });
      const capture = () => posthog.capture('$pageview');
      Router.events.on('routeChangeComplete', capture);
      return () => Router.events.off('routeChangeComplete', capture);
    }
  }, []);

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
      <Hydrate state={pageProps.dehydratedState}>
        <ThemeProvider theme={theme}>
          <ConfirmProvider defaultOptions={{
            title: 'Jste si jistí?',
            cancellationText: 'Zrušit',
            confirmationButtonProps: { autoFocus: true }
          }}>
            <LocalizationProvider dateAdapter={AdapterDateFns}>
              <SnackbarProvider maxSnack={3}>
                <ProvideAuth>
                  <ProvideMeta>
                    <Layout>
                      <Component {...pageProps} />
                    </Layout>
                  </ProvideMeta>
                </ProvideAuth>
              </SnackbarProvider>
            </LocalizationProvider>
          </ConfirmProvider>
        </ThemeProvider>
      </Hydrate>
    </QueryClientProvider>
  );
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
