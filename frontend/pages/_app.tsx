import { configureUrql } from '@/graphql/query';
import { ConfirmProvider } from '@/ui/Confirm';
import { Tracking } from '@/ui/Tracking';
import { UserRefresher } from '@/ui/use-auth';
import i18next from 'i18next';
import NextAdapterPages from 'next-query-params/pages';
import { withUrqlClient } from 'next-urql';
import { AppProps, NextWebVitalsMetric } from 'next/app';
import Router from 'next/router';
import { event } from 'nextjs-google-analytics';
import NProgress from 'nprogress';
import csZodTranslation from '@/public/locales/cs/zod.json';
import * as React from 'react';
import { ToastContainer } from 'react-toastify';
import { QueryParamProvider } from 'use-query-params';
import { z } from 'zod';
import { makeZodI18nMap } from 'zod-i18n-map';
import { Analytics } from "@vercel/analytics/react"
import { UpdateNotifier } from '@/ui/UpdateNotifier';
import { Provider, createStore } from 'jotai';
import { storeRef } from '@/ui/state/auth';
import { ErrorNotifier } from '@/ui/ErrorNotifier';
import dynamic from 'next/dynamic';

import 'glider-js/glider.min.css';
import 'nprogress/nprogress.css';
import 'react-toastify/dist/ReactToastify.css';
import '../lite-youtube-embed.css';
import '../index.css';
import '../leaflet.css';
import '../calendar.css';

const SpeedInsights = dynamic(
  () => import('@vercel/speed-insights/next').then((x) => x.SpeedInsights),
  { ssr: false },
);

NProgress.configure({ template: '<div role="bar" style="display:none"></div><div class="spinner" role="spinner"><div class="spinner-icon"></div></div>' });
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

function App({ Component, pageProps, resetUrqlClient }: AppProps & {
  resetUrqlClient: () => void;
}) {
  storeRef.resetUrqlClient = resetUrqlClient;
  if (typeof window === 'undefined') {
    storeRef.current = createStore();
  }

  return (
    <QueryParamProvider adapter={NextAdapterPages} options={{ removeDefaultsFromUrl: true }}>
      <Provider store={storeRef.current}>
        <ConfirmProvider>
          <Tracking />
          <Analytics />
          <SpeedInsights />
          <Component {...pageProps} />
          <UpdateNotifier />
          <ErrorNotifier />
          <UserRefresher />
          <ToastContainer limit={3} />
        </ConfirmProvider>
      </Provider>
    </QueryParamProvider>
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
