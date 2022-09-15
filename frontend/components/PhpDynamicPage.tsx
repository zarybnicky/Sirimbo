import * as React from 'react';
import { Container, Typography } from '@mui/material';
import { GetServerSideProps, GetServerSidePropsContext, GetServerSidePropsResult } from 'next';
import { origin } from 'lib/query';
import { NextRequest } from 'next/server';

export type PhpPage = {
  meta: string;
  title: string;
  subheader: string;
  messages: { type: string; text: string; }[];
  content: string;
};

export const PhpPageView: React.FC<{ page: PhpPage; }> = ({ page }) => {
  return <Container maxWidth="lg" style={{ marginTop: 80 }}>
    {page.title && page.title !== 'TK Olymp' && (
      <div className="header-section">
        <div className="container full">
          <h1>{page.title}</h1>
          {page.subheader}
        </div>
      </div>
    )}

    {page.messages.map((msg, i) => (
      <div className="container" key={i}>
        <div className={`alert alert-${msg.type}`} dangerouslySetInnerHTML={{ __html: msg.text }} />
      </div>
    ))}

    <Typography variant="h3" component="h2">{page.title}</Typography>

    <div dangerouslySetInnerHTML={{ __html: page.content }} />
  </Container>;
}

export const getServerSidePhpPage: GetServerSideProps = async ({ req, resolvedUrl }) => {
  try {
    const page = await fetchPhpPage(resolvedUrl, { req });
    console.log(page);
    if (page.type === 'redirect') {
      return {
        redirect: {
          statusCode: page.redirect,
          destination: page.redirectUrl,
        },
      } as GetServerSidePropsResult<any>;
    }
    if (page.type === 'error') {
      return {
        redirect: {
          permanent: false,
          destination: '/error',
        },
      } as GetServerSidePropsResult<any>;
    }
    return {
      props: {
        page: page.content,
      },
    } as GetServerSidePropsResult<any>;
  } catch (e) {
    console.log(e);
    return { notFound: true };
  }
};

type PhpPageResponse
  = { type: 'redirect'; redirect: number, redirectUrl: string }
  | { type: 'success'; content: PhpPage }
  | { type: 'error'; error: number, errorText: string };

export const fetchPhpPage = async (url: string, options: {
  body?: FormData,
  req?: GetServerSidePropsContext['req'],
}): Promise<PhpPageResponse> => {
  const cookie = options?.req?.headers.cookie

  const res = await fetch(`${origin}/old${url}`, {
    credentials: 'include',
    headers: cookie ? { Cookie: cookie } : undefined,
    method: options?.body ? 'POST' : 'GET',
    redirect: 'follow',
    body: options?.body,
  });

  if (res.redirected) {
    return {
      type: 'redirect',
      redirect: 303,
      redirectUrl: res.url,
    }
  }

  if (!res.ok) {
    return {
      type: 'error',
      error: res.status,
      errorText: res.statusText,
    }
  }

  return {
    type: 'success',
    content: await res.json() as PhpPage,
  }
};
