import React from 'react';
import Link from 'next/link';
import parse, {
  domToReact,
  DOMNode,
  Element,
  HTMLReactParserOptions,
} from 'html-react-parser';

interface Props {
  value: string | undefined | null;
  className?: string;
  style?: React.CSSProperties;
}

export const RichTextView = ({ value, className, style }: Props) => {
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
        ...style
      }}
    >
      {parse(value, options)}
    </div>
  );
};

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

    if (domNode.name === 'a') {
      const { href, class: className, ...rest } = domNode.attribs;
      return (
        <Link href={href as any || '#'} className={className} {...rest}>
          {domToReact(domNode.children as DOMNode[])}
        </Link>
      );
    }

    if (domNode.name === 'input') {
      if (domNode.attribs.value === '') {
        delete domNode.attribs.value;
      }
      return domNode;
    }
  },
};
