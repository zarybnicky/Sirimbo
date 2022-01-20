import * as React from 'react';
import { CssBaseline } from '@material-ui/core';
import { Header } from './Header';
import { Footer } from './Footer';

export const Layout = ({ children }: { children: React.ReactChild | React.ReactChildren; }) => <React.Fragment>
  <CssBaseline />
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
</React.Fragment>;
