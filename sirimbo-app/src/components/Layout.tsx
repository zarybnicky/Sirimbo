import * as React from 'react';
import { CssBaseline } from '@material-ui/core';
import { Notification } from 'ra-ui-materialui';
import { AppHeader } from './AppHeader';
import { AppFooter } from './AppFooter';

export const Layout = ({ children }: { children: React.ReactChild | React.ReactChildren; }) => <React.Fragment>
  <CssBaseline />
  <AppHeader />
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
  <AppFooter />
  <Notification />
</React.Fragment>;
