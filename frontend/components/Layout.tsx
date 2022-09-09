import React from 'react'
import Head from 'next/head'
import { Header } from './Header';
import { Footer } from './Footer';

export const Layout: React.FC<{
  title: string;
}> = ({ children, title }) => <>
  <Head>
    <title>{title}</title>
    <meta charSet="utf-8" />
    <meta name="viewport" content="initial-scale=1.0, width=device-width" />
  </Head>
  <Header />
  <React.Suspense fallback={
    <div style={{
      display: "flex",
      alignItems: "center",
      flexDirection: "column",
      justifyContent: "center",
      height: "100vh",
    }}>Načítám...</div>
  }>
    {children}
  </React.Suspense>
  <Footer />
</>;
