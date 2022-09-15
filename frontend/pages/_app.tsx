import * as React from "react";
import { AppProps, NextWebVitalsMetric } from 'next/app';
import Router, { useRouter } from "next/router";
import NProgress from "nprogress";
import "nprogress/nprogress.css";
import { GoogleAnalytics, event } from "nextjs-google-analytics";
import Head from 'next/head'
import { ProvideAuth } from 'lib/data/use-auth';
import { ThemeProvider } from "@mui/material";
import { theme } from "lib/theme";
import 'bootstrap/dist/css/bootstrap.min.css';
import "public/style/index.scss";
import 'public/style/material-icons.css';
import '@react-page/editor/lib/index.css';
import '@react-page/plugins-slate/lib/index.css';
import '@react-page/plugins-image/lib/index.css';
import { SnackbarProvider } from "notistack";
import { LocalizationProvider } from "@mui/x-date-pickers/LocalizationProvider";
import { AdapterDateFns } from '@mui/x-date-pickers/AdapterDateFns';
import { useCookie } from "lib/use-cookie";
import { Hydrate, QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { persistQueryClient } from '@tanstack/react-query-persist-client';
import { createSyncStoragePersister } from '@tanstack/query-sync-storage-persister';
import { NewLayout } from "components/NewLayout";
import { OldLayout } from "components/OldLayout";

Router.events.on("routeChangeStart", () => NProgress.start());
Router.events.on("routeChangeComplete", () => NProgress.done());
Router.events.on("routeChangeError", () => NProgress.done());

export default function App({ Component, pageProps }: AppProps) {
  const [layout, setLayout] = useCookie('layout', 'old');
  const Layout = layout === 'new' ? NewLayout : OldLayout;
  const router = useRouter()

  const queryClientRef = React.useRef<QueryClient>()
  if (!queryClientRef.current) {
    queryClientRef.current = new QueryClient({
      defaultOptions: {
        queries: {
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

  React.useEffect(() => {
    import('react-facebook-pixel')
      .then((x) => x.default)
      .then((ReactPixel) => {
        ReactPixel.init('704526480597551');
        ReactPixel.pageView();
        router.events.on('routeChangeComplete', () => ReactPixel.pageView());
      })
  }, [router.events]);

  return (
    <QueryClientProvider client={queryClientRef.current}>
      <Hydrate state={pageProps.dehydratedState}>
        <ThemeProvider theme={theme}>
          <LocalizationProvider dateAdapter={AdapterDateFns}>
            <SnackbarProvider maxSnack={3}>
              <ProvideAuth>
                <Head>
                  <meta charSet="utf-8" />
                  <meta name="viewport" content="initial-scale=1.0, width=device-width" />
                  <meta name="keywords" content="taneční klub, tk, olymp, olomouc, tk olymp, sportovní tanec" />
                  <meta name="ICBM" content="49.591700,17.285174" />
                  <meta name="geo.placename" content="Olomouc, Česká Republika" />
                  <meta name="geo.position" content="49.591700;17.285174" />
                  <meta name="geo.region" content="cs" />
                  <meta name="wot-verification" content="ec0cf41ab42dae52d3d4" />
                  <meta name="msvalidate.01" content="7BD6C8B5748FC22EF06AB3AE89900885" />
                  <meta name="facebook-domain-verification" content="k8tt64a93roxiymxo79clpvklan9j2" />
                  <meta name="google-site-verification" content="Hfe7zlgTDOIpJv4rKGQz2Xg8Aezb6sIO0aAxVhrml9w" />
                  <meta name="norton-safeweb-site-verification" content="r44xj2vskhlgkyqcqm1hdgga2jdfj-idvyys0277y96s72k-tq0z-yyjdu7h3el6pi2gek0i4ykq3xgiguufrvuhj8nbj4n4miwjhvumhp35jfrafyynhj4ee8ctzpzh" />
                  <meta property="fb:app_id" content="132983570203245" />
                  <link rel="shortcut icon" type="image/x-icon" href="/favicon.ico" />
                </Head>
                <GoogleAnalytics gaMeasurementId="UA-44456908-1" trackPageViews={{ ignoreHashChange: true }} />
                <Layout layout={layout || 'old'} setLayout={setLayout}>
                  <Component {...pageProps} />
                </Layout>
              </ProvideAuth>
            </SnackbarProvider>
          </LocalizationProvider>
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
