import React, { ReactNode } from 'react'
import Link from 'next/link'
import Head from 'next/head'

type Props = {
  children?: ReactNode
  title?: string
}

const Layout = ({ children, title = 'This is the default title' }: Props) => (
  <div>
    <Head>
      <title>{title}</title>
      <meta charSet="utf-8" />
      <meta name="viewport" content="initial-scale=1.0, width=device-width" />
    </Head>
    <header>
      <nav>
        <Link href="/">
          <a>Home</a>
        </Link>{' '}
        |{' '}
        <Link href="/about">
          <a>About</a>
        </Link>{' '}
        |{' '}
        <Link href="/users">
          <a>Users List</a>
        </Link>{' '}
        | <a href="/api/users">Users API</a>
      </nav>
    </header>
    {children}
    <footer>
      <hr />
      <span>I'm here to stay (Footer)</span>
    </footer>
  </div>
)

export default Layout


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
