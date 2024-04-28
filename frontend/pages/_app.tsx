import { configureUrql } from '@/graphql/query';
import { ConfirmProvider } from '@/ui/Confirm';
import { Tracking } from '@/ui/Tracking';
import { ProvideAuth } from '@/ui/use-auth';
import i18next from 'i18next';
import NextAdapterPages from 'next-query-params/pages';
import { withUrqlClient } from 'next-urql';
import { AppProps, NextWebVitalsMetric } from 'next/app';
import Router from 'next/router';
import { event } from 'nextjs-google-analytics';
import NProgress from 'nprogress';
import csZodTranslation from '@/public/locales/cs/zod.json';
import * as React from 'react';
import { ToastContainer, toast } from 'react-toastify';
import { TypedEventTarget } from 'typescript-event-target';
import { CombinedError } from 'urql';
import { QueryParamProvider } from 'use-query-params';
import { z } from 'zod';
import { makeZodI18nMap } from 'zod-i18n-map';
import * as Sentry from '@sentry/nextjs';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';

import 'glider-js/glider.min.css';
import 'nprogress/nprogress.css';
import 'react-toastify/dist/ReactToastify.css';
import '../lite-youtube-embed.css';
import '../index.css';
import '../leaflet.css';
import '../calendar.css';
import { UpdateNotifier } from '@/ui/UpdateNotifier';
import { Provider, createStore } from 'jotai';
import { storeRef } from '@/ui/auth/state';

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

const errorTarget = new TypedEventTarget<{ error: CustomEvent<CombinedError> }>()

function App({ Component, pageProps, resetUrqlClient }: AppProps & {
  resetUrqlClient: () => void;
}) {
  if (typeof window === 'undefined') {
    storeRef.current = createStore();
  }

  React.useEffect(() => {
    const onError = ({ detail: ex }: CustomEvent<CombinedError>) => {
      if (ex.message === '[GraphQL] INVALID_CREDENTIALS') {
        toast.error('Neplatné přihlašovací údaje');
      } else if (ex.message === '[GraphQL] duplicate key value violates unique constraint "users_email_key"') {
        toast.error('Zřejmě již v systému máte účet. Přihlašte se a vyplňte si přihlášku v sekci "Profil"');
      } else {
        toast.error(ex.message);
        Sentry.captureException(ex);
      }
    };
    errorTarget.addEventListener('error', onError);
    return () => errorTarget.removeEventListener('error', onError);
  }, []);

  useLayoutEffect(() => {
    // Prevent Sentry spam
    window.addEventListener('error', function(e) {
      if (e.message === "ResizeObserver loop completed with undelivered notifications.") {
        console.log(e)
        e.stopImmediatePropagation();
        e.preventDefault();
      }
    }, true);
  }, []);

  return (
    <QueryParamProvider adapter={NextAdapterPages} options={{ removeDefaultsFromUrl: true }}>
      <Provider store={storeRef.current}>
      <ProvideAuth onReset={resetUrqlClient}>
        <ConfirmProvider>
          <Tracking />
          <Component {...pageProps} />
          <UpdateNotifier />
          <ToastContainer limit={3} />
        </ConfirmProvider>
      </ProvideAuth>
      </Provider>
    </QueryParamProvider>
  );
}

export default withUrqlClient(configureUrql(errorTarget), { ssr: false })(App);

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
