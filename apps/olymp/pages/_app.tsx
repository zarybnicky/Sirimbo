import { configureUrql } from '@app/graphql/query';
import { ConfirmProvider } from '@app/ui/Confirm';
import { Tracking } from '@app/ui/Tracking';
import { ProvideAuth } from '@app/ui/use-auth';
import i18next from 'i18next';
import NextAdapterPages from 'next-query-params/pages';
import { withUrqlClient } from 'next-urql';
import { AppProps, NextWebVitalsMetric } from 'next/app';
import Router from 'next/router';
import { event } from 'nextjs-google-analytics';
import NProgress from 'nprogress';
import csZodTranslation from 'public/locales/cs/zod.json';
import * as React from 'react';
import { ToastContainer, toast } from 'react-toastify';
import { TypedEventTarget } from 'typescript-event-target';
import { CombinedError } from 'urql';
import { QueryParamProvider } from 'use-query-params';
import { z } from 'zod';
import { makeZodI18nMap } from 'zod-i18n-map';
import * as Sentry from '@sentry/nextjs';

import 'glider-js/glider.min.css';
import 'nprogress/nprogress.css';
import 'react-toastify/dist/ReactToastify.css';
import '../calendar.css';
import '../index.css';

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
  React.useEffect(() => {
    const onError = ({ detail: ex }: CustomEvent<CombinedError>) => {
      toast.error(ex.message);
      Sentry.captureException(ex);
    };
    errorTarget.addEventListener('error', onError);
    return () => errorTarget.removeEventListener('error', onError);
  }, []);
  return (
    <QueryParamProvider adapter={NextAdapterPages} options={{ removeDefaultsFromUrl: true }}>
      <ProvideAuth onReset={resetUrqlClient}>
        <ConfirmProvider>
          <Tracking />
          <Component {...pageProps} />
          <ToastContainer limit={3} />
        </ConfirmProvider>
      </ProvideAuth>
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
