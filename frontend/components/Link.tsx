import * as React from "react"
import NextLink, { LinkProps as NextLinkProps } from 'next/link';
import MuiLink, { LinkProps as MuiLinkProps } from '@mui/material/Link';
import { useRouter } from "next/router"

export function isRelative(url: string) {
  url = url.replace(/^https:\/\/(www\.)?tkolymp.cz\//, '');
  return !new RegExp("^(?:[a-z]+:)?//", "i").test(url)
}

interface OldLinkProps extends NextLinkProps {
  href: string
  children: React.ReactElement
}

function OldLink({ href, passHref, as, children, ...props }: OldLinkProps) {
  const router = useRouter()

  if (!href) {
    return null
  }

  href = href.replace(/^https:\/\/(www\.)?tkolymp.cz\//, '');

  // Use Next Link for internal links, and <a> for others.
  if (isRelative(href)) {
    href = href.replace(/^\/cs/, '')
    // Disable prefetching in preview mode.
    // We do this here inside of inline `prefetch={!router.isPreview}`
    // because `prefetch={true}` is not allowed.
    // See https://nextjs.org/docs/messages/prefetch-true-deprecated
    const linkProps = router.isPreview ? { prefetch: false, ...props } : props

    return (
      <NextLink as={as} href={href} passHref={passHref} {...linkProps}>
        {children}
      </NextLink>
    )
  }

  return React.cloneElement(children, { href, target: "_blank", rel: "noreferrer" });
}



interface NextLinkComposedProps
  extends Omit<React.AnchorHTMLAttributes<HTMLAnchorElement>, 'href'>,
  Omit<NextLinkProps, 'onClick' | 'onMouseEnter' | 'onTouchStart'> {
}

export const NextLinkComposed = React.forwardRef<HTMLAnchorElement, NextLinkComposedProps>(
  function NextLinkComposed(props, ref) {
    const { href, as, replace, scroll, shallow, prefetch, locale, ...other } = props;

    return (
      <NextLink
        href={href}
        prefetch={prefetch}
        as={as}
        replace={replace}
        scroll={scroll}
        shallow={shallow}
        passHref
        locale={locale}
      >
        <a ref={ref} {...other} />
      </NextLink>
    );
  },
);

export type LinkProps = {
  activeClassName?: string;
  as?: NextLinkProps['as'];
  href: NextLinkProps['href'];
  linkAs?: NextLinkProps['as']; // Useful when the as prop is shallow by styled().
  noLinkStyle?: boolean;
} & Omit<NextLinkComposedProps, 'to' | 'linkAs' | 'href'> &
  Omit<MuiLinkProps, 'href'>;

// A styled version of the Next.js Link component:
// https://nextjs.org/docs/api-reference/next/link
const Link = React.forwardRef<HTMLAnchorElement, LinkProps>(function Link(props, ref) {
  const {
    activeClassName = 'active',
    as,
    className: classNameProps,
    href,
    locale,
    noLinkStyle,
    prefetch,
    replace,
    role, // Link don't have roles.
    scroll,
    shallow,
    ...other
  } = props;

  const router = useRouter();
  const pathname = typeof href === 'string' ? href : href.pathname;
  let className = classNameProps;
  if (router.pathname === pathname && activeClassName) {
    className += ' ' + activeClassName;
  }

  const isExternal =
    typeof href === 'string' && (href.indexOf('http') === 0 || href.indexOf('mailto:') === 0);

  if (isExternal) {
    if (noLinkStyle) {
      return <a className={className} href={href} ref={ref} {...other} />;
    }

    return <MuiLink className={className} href={href} ref={ref} {...other} />;
  }

  const nextjsProps = { as, replace, scroll, shallow, prefetch, locale };

  if (noLinkStyle) {
    return <NextLinkComposed href={href} className={className} ref={ref} {...nextjsProps} {...other} />;
  }

  return (
    <MuiLink
      href={typeof href === 'string' ? href : href ? href.toString() : href}
      component={NextLinkComposed}
      className={className}
      ref={ref}
      {...nextjsProps}
      {...other}
    />
  );
});

export default Link;
