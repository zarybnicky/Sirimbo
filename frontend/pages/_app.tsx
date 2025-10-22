import { configureUrql } from '@/graphql/query';
import { ConfirmProvider } from '@/ui/Confirm';
import { ErrorNotifier } from '@/ui/ErrorNotifier';
import { FillYourProfileReminder } from '@/ui/FillYourProfileReminder';
import { storeRef, tenantIdAtom } from '@/ui/state/auth';
import { Tracking } from '@/ui/Tracking';
import { UpdateNotifier } from '@/ui/UpdateNotifier';
import { UserRefresher } from '@/ui/use-auth';
import { Analytics } from "@vercel/analytics/react";
import { Provider, createStore } from 'jotai';
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
import { cs } from "zod/locales"
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { getCookie } from 'cookies-next/client';

import 'glider-js/glider.min.css';
import 'nprogress/nprogress.css';
import 'react-toastify/dist/ReactToastify.css';
import '../style/calendar.css';
import '../style/index.css';
import '../style/leaflet.css';
import '../style/lite-youtube-embed.css';

NProgress.configure({ template: '<div role="bar" style="display:none"></div><div class="spinner" role="spinner"><div class="spinner-icon"></div></div>' });
Router.events.on('routeChangeStart', () => NProgress.start());
Router.events.on('routeChangeComplete', () => NProgress.done());
Router.events.on('routeChangeError', () => NProgress.done());

z.config(cs());

function App({ Component, pageProps, resetUrqlClient }: AppProps & {
  resetUrqlClient: () => void;
}) {
  storeRef.resetUrqlClient = resetUrqlClient;
  // eslint-disable-next-line unicorn/prefer-global-this
  if (typeof window === 'undefined') {
    storeRef.current = createStore();
  }

  useLayoutEffect(() => {
    const newTenantId = String(getCookie('tenant_id'));
    storeRef.current.set(tenantIdAtom, newTenantId);
    const root = document.querySelector('body');
    if (root) {
      root.classList.forEach((cls) => {
        if (cls.includes('tenant-'))
          root.classList.remove(cls);
      });
      root.classList.add(`tenant-${newTenantId}`);
    }
  }, []);

  return (
    <QueryParamProvider adapter={NextAdapterPages} options={{ removeDefaultsFromUrl: true }}>
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
