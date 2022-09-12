import * as React from "react";
import { AppProps, NextWebVitalsMetric } from 'next/app';
import Router from "next/router";
import NProgress from "nprogress";
import "nprogress/nprogress.css";
import { GoogleAnalytics, event } from "nextjs-google-analytics";
import { ApolloProvider } from '@apollo/client';
import { client } from "lib/apollo";
import Head from 'next/head'
import { Header } from 'components/Header';
import { Footer } from 'components/Footer';
import { ProvideAuth } from 'lib/data/use-auth';

import "public/style/index.scss";
import 'public/style/material-icons.css';
import '@react-page/editor/lib/index.css';
import '@react-page/plugins-slate/lib/index.css';
import '@react-page/plugins-image/lib/index.css';
import { ThemeProvider } from "@mui/material";
import { theme } from "lib/theme";

Router.events.on("routeChangeStart", () => NProgress.start());
Router.events.on("routeChangeComplete", () => NProgress.done());
Router.events.on("routeChangeError", () => NProgress.done());

export default function MyApp({ Component, pageProps }: AppProps) {
  return (
    <ThemeProvider theme={theme}>
      <ApolloProvider client={client}>
        <ProvideAuth>
          <Head>
            <meta charSet="utf-8" />
            <meta name="viewport" content="initial-scale=1.0, width=device-width" />
          </Head>
          <GoogleAnalytics trackPageViews={{ ignoreHashChange: true }} />
          <Header />
          <Component {...pageProps} />
          <Footer />
        </ProvideAuth>
      </ApolloProvider>
    </ThemeProvider>
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
