import { configureUrql } from '@/lib/query';
import { ConfirmProvider } from '@/ui/Confirm';
import { ErrorNotifier } from '@/ui/ErrorNotifier';
import { FillYourProfileReminder } from '@/ui/FillYourProfileReminder';
import { setNewTenant, storeRef } from '@/ui/state/auth';
import { Tracking } from '@/ui/Tracking';
import { UpdateNotifier } from '@/ui/UpdateNotifier';
import { UserRefresher } from '@/ui/use-auth';
import { Analytics } from '@vercel/analytics/react';
import { createStore, Provider } from 'jotai';
import NextAdapterPages from 'next-query-params/pages';
import { withUrqlClient } from 'next-urql';
import type { AppProps, NextWebVitalsMetric } from 'next/app';
import Router from 'next/router';
import { event } from 'nextjs-google-analytics';
import NProgress from 'nprogress';
import * as React from 'react';
import { ToastContainer } from 'react-toastify';
import { QueryParamProvider } from 'use-query-params';
import { z } from 'zod';
import { cs } from 'zod/locales';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { getCookie, setCookie } from 'cookies-next/client';
import { SpeedInsights } from '@vercel/speed-insights/next';

import 'core-js/actual/array/to-reversed';
import 'core-js/actual/array/to-sorted';

import 'glider-js/glider.min.css';
import 'nprogress/nprogress.css';
import 'react-toastify/dist/ReactToastify.css';
import 'react-data-grid/lib/styles.css';
import '../style/calendar.css';
import '../style/index.css';
import '../style/leaflet.css';
import '../style/lite-youtube-embed.css';
import { hostToTenantId } from '@/tenant/catalog-server';

NProgress.configure({
  template:
    '<div role="bar" style="display:none"></div><div class="spinner" role="spinner"><div class="spinner-icon"></div></div>',
});
Router.events.on('routeChangeStart', () => NProgress.start());
Router.events.on('routeChangeComplete', () => NProgress.done());
Router.events.on('routeChangeError', () => NProgress.done());

z.config(cs());

function App({
  Component,
  pageProps,
  resetUrqlClient,
}: AppProps & {
  resetUrqlClient: () => void;
}) {
  // eslint-disable-next-line react-hooks/immutability
  storeRef.resetUrqlClient = resetUrqlClient;
  if (typeof window === 'undefined') {
    // eslint-disable-next-line react-hooks/immutability
    storeRef.current = createStore();
  }

  useLayoutEffect(() => {
    const origin = new URL(window.origin);
    const tenantId = hostToTenantId.get(origin.host);
    const existing = String(getCookie('tenant_id'));

    if (tenantId !== existing && tenantId) {
      setCookie('tenant_id', String(tenantId), {
        path: '/',
        sameSite: 'lax',
        secure: origin.protocol === 'https:',
        domain: origin.host,
        expires: new Date(Date.now() + 1000 * 60 * 60 * 24 * 365 * 10),
      });
    }
    setNewTenant(String(getCookie('tenant_id')));
  }, []);

  return (
    <QueryParamProvider
      adapter={NextAdapterPages}
      options={{ removeDefaultsFromUrl: true }}
    >
      <Provider store={storeRef.current}>
        <ConfirmProvider>
          <Tracking />
          <Analytics />
          <Component {...pageProps} />
          <UpdateNotifier />
          <FillYourProfileReminder />
          <ErrorNotifier />
          <UserRefresher />
          <ToastContainer limit={3} />
          <SpeedInsights />
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
