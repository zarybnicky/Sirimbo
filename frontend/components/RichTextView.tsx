import Link from 'next/link';
import parse, {
  domToReact,
  DOMNode,
  Element,
  HTMLReactParserOptions,
} from 'html-react-parser';

interface Props {
  value: string; // | any[] | object | undefined | null;
  className?: string;
}

export const RichTextView = ({ value, className }: Props) => {
  if (!value || (Array.isArray(value) && !value.length)) {
    return null;
  }
  /* if (Array.isArray(value)) {
   *   return (
   *     <div className={className}>
   *       <SlateEditor name="viewOnly" readOnly value={value} />
   *     </div>
   *   );
   * }
   * if (typeof value === 'object') {
   *   return <>Neplatný obsah, nahlašte to prosím správci obsahu</>;
   * }
   * if (value.startsWith('[')) {
   *   return (
   *     <div className={className}>
   *       <SlateEditor name="viewOnly" readOnly value={JSON.parse(value) as any[]} />
   *     </div>
   *   );
   * } */
  return (
    <div
      className={`prose ${className}`}
      style={{
        overflowWrap: 'break-word',
        wordWrap: 'break-word',
        wordBreak: 'break-word',
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
