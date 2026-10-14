import React from 'react';
import Link from 'next/link';
import Image from '@/ui/Image';
import parse, {
  domToReact,
  type DOMNode,
  type Element,
  type HTMLReactParserOptions,
  attributesToProps,
} from 'html-react-parser';

interface Props {
  value: string | undefined | null;
  className?: string;
  style?: React.CSSProperties;
}

export function RichTextView({ value, className, style }: Props) {
  if (!value || !value.trim() || value === '[]') {
    return null;
  }
  return (
    <div
      className={`prose prose-accent ${className || ''}`}
      style={{
        overflowWrap: 'break-word',
        wordWrap: 'break-word',
        wordBreak: 'break-word',
        ...style,
      }}
    >
      {parse(value, options)}
    </div>
  );
}

const isElement = (domNode: DOMNode): domNode is Element => {
  const isTag = domNode.type === 'tag';
  const hasAttributes = (domNode as Element).attribs !== undefined;
  return isTag && hasAttributes;
};

const options: HTMLReactParserOptions = {
  replace: (domNode) => {
    if (!isElement(domNode)) {
      return;
    }

    if (domNode.name === 'img') {
      const { src, alt, class: className, ...rest } = domNode.attribs;
      const width = Number(rest.width);
      const height = Number(rest.height);
      const props = {
        loading: 'lazy' as const,
        decoding: 'async' as const,
        ...attributesToProps(rest),
        src,
        className,
      };
      return (
        <a href={src} target="_blank" rel="noreferrer">
          {src?.startsWith('/f/') && !rest.srcset &&
          Number.isSafeInteger(width) && width > 0 &&
          Number.isSafeInteger(height) && height > 0 ? (
            <Image {...props} src={src} alt={alt ?? ''} width={width} height={height} />
          ) : (
            // eslint-disable-next-line @next/next/no-img-element -- Preserve external sources, authored srcsets, and images without valid dimensions.
            <img {...props} alt={alt ?? ''} />
          )}
        </a>
      );
    }

    if (domNode.name === 'a') {
      const { href, class: className, ...rest } = domNode.attribs;
      return (
        <Link
          href={(href as any) || '#'}
          className={className}
          {...attributesToProps(rest)}
        >
          {domToReact(domNode.children as DOMNode[])}
        </Link>
      );
    }

    if (domNode.name === 'input') {
      if (domNode.attribs.value === '') {
        domNode.attribs.value = undefined as any as string;
      }
      return domNode;
    }
  },
};
