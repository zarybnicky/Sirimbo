import * as React from 'react';
import NextLink, { LinkProps as NextLinkProps } from 'next/link';
import { useRouter } from 'next/router';

export function isRelative(url: string) {
  url = url.replace(/^https:\/\/(www\.)?tkolymp.cz\//, '');
  return !new RegExp('^(?:[a-z]+:)?//', 'i').test(url);
}

interface OldLinkProps extends NextLinkProps {
  href: string;
  children: React.ReactElement;
}

function OldLink({ href, passHref, as, children, ...props }: OldLinkProps) {
  const router = useRouter();

  if (!href) {
    return null;
  }

  href = href.replace(/^https:\/\/(www\.)?tkolymp.cz\//, '');

  // Use Next Link for internal links, and <a> for others.
  if (isRelative(href)) {
    href = href.replace(/^\/cs/, '');
    // Disable prefetching in preview mode.
    // We do this here inside of inline `prefetch={!router.isPreview}`
    // because `prefetch={true}` is not allowed.
    // See https://nextjs.org/docs/messages/prefetch-true-deprecated
    const linkProps = router.isPreview ? { prefetch: false, ...props } : props;

    return (
      <NextLink as={as} href={href} passHref={passHref} {...linkProps}>
        {children}
      </NextLink>
    );
  }

  return React.cloneElement(children, { href, target: '_blank', rel: 'noreferrer' });
}
