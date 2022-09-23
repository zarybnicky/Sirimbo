import * as React from 'react';
import { Container, Typography } from '@mui/material';
import { GetServerSideProps, GetServerSidePropsContext, GetServerSidePropsResult } from 'next';
import { origin } from 'lib/query';
import parse, { domToReact, DOMNode, Element, HTMLReactParserOptions } from "html-react-parser"
import { useRouter } from 'next/router';
import { useSnackbar } from 'notistack';
import { NextLinkComposed, isRelative } from "./Link"
import Image from "next/image"
import { useAuth } from 'lib/data/use-auth';

export type PhpPage = {
  meta: string;
  title: string;
  subheader: string;
  messages: { type: string; text: string; }[];
  content: string;
};

const isElement = (domNode: DOMNode): domNode is Element => {
  const isTag = domNode.type === "tag";
  const hasAttributes = (domNode as Element).attribs !== undefined;

  return isTag && hasAttributes;
};

const options = (onSubmit: (e: React.FormEvent<HTMLFormElement>) => void): HTMLReactParserOptions => ({
  replace: (domNode) => {
    if (isElement(domNode)) {
      if (domNode.name === "form") {
        const { class: className, ...rest } = domNode.attribs;
        return (
          <form onSubmit={onSubmit} className={className} {...rest}>
            {domToReact(domNode.children as DOMNode[], options(onSubmit))}
          </form>
        );
      }

      if (domNode.name === "img") {
        const { class: className, src, alt, width = "100px", height = "100px" } = domNode.attribs
        if (src && isRelative(src)) {
          return (
            <Image
              src={src}
              width={`${width}px`}
              height={`${height}px`}
              alt={alt}
              className={className}
              layout="intrinsic"
              objectFit="cover"
            />
          )
        }
      }

      if (domNode.name === "a") {
        const { href, class: className, ...rest } = domNode.attribs
        return (
          <NextLinkComposed href={href || '#'} className={className} {...rest}>
            {domToReact(domNode.children as DOMNode[])}
          </NextLinkComposed>
        );
      }

      if (domNode.name === "input") {
        if (domNode.attribs.value === "") {
          delete domNode.attribs.value
        }

        return domNode
      }

      if (domNode.name === 'option') {
        return <option {...domNode.attribs}>
          {domToReact(domNode.children as DOMNode[])}
        </option>;
      }
    }
  },
});

export const PhpPageView: React.FC<{ page: PhpPage; }> = ({ page: initialPage }) => {
  const { user, isLoading } = useAuth();
  const router = useRouter();
  const { enqueueSnackbar } = useSnackbar();
  if (!user && !isLoading) {
    enqueueSnackbar('Nejprve se musíte přihlásit', { variant: 'error' })
    router.push('/login');
  }
  const [page, setPage] = React.useState(initialPage);

  React.useEffect(() => {
    setPage(initialPage);
  }, [initialPage]);

  const submitForm = React.useCallback(async (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    const form = e.currentTarget;
    if (form.method === 'post') {
      const body = new FormData(form);
      const action = (form.action || router.asPath)
        .replace(origin, '')
        .replace(window.location.origin, '');

      const result = await fetchPhpPage(action, { body });
      if (result.type === 'error') {
        enqueueSnackbar(result.errorText, { variant: 'error' });
      } else if (result.type === 'redirect') {
        router.push(result.redirectUrl.replace(origin, ''));
      } else {
        setPage(result.content);
      }
    }
  }, []);

  return (
    <Container maxWidth="lg" sx={{ margin: '80px auto' }}>
      {page.messages.map((msg, i) => (
        <div key={i} className={`alert alert-${msg.type}`} dangerouslySetInnerHTML={{ __html: msg.text }} />
      ))}

      <Typography variant="h3" component="h2">{page.title}</Typography>
      {page.subheader}

      <div className='prose' style={{
        overflowWrap: "break-word",
        wordWrap: "break-word",
        wordBreak: "break-word",
      }}>
        {parse(page.content, options(submitForm))}
      </div >
    </Container>
  );
}

export const getServerSidePhpPage: GetServerSideProps = async ({ req, resolvedUrl }) => {
  try {
    const page = await fetchPhpPage(resolvedUrl, { req });
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
    console.log('redirect', res.url);
    return {
      type: 'redirect',
      redirect: 303,
      redirectUrl: res.url,
    }
  }

  if (!res.ok) {
    console.log('error', res.statusText);
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

export default PhpPageView;
export { getServerSidePhpPage as getServerSideProps };
