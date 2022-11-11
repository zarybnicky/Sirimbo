import { NextLinkComposed } from "./Link"
import parse, { domToReact, DOMNode, Element, HTMLReactParserOptions } from "html-react-parser"

const isElement = (domNode: DOMNode): domNode is Element => {
  const isTag = domNode.type === "tag";
  const hasAttributes = (domNode as Element).attribs !== undefined;

  return isTag && hasAttributes;
};

const options: HTMLReactParserOptions = {
  replace: (domNode) => {
    if (!isElement(domNode)) {
      return;
    }

    if (domNode.name === "a") {
      const { href, class: className, ...rest } = domNode.attribs
      return (
        <NextLinkComposed href={href || '#'} className={className} {...rest}>
          {domToReact(domNode.children as DOMNode[])}
        </NextLinkComposed>
      );
    }
  },
};

export const HtmlView: React.FC<{
  content: string;
  className?: string;
}> = ({ content, className }) => {
  return (
    <div className={`prose ${className}`} style={{
      overflowWrap: "break-word",
      wordWrap: "break-word",
      wordBreak: "break-word",
    }}>
      {parse(content, options)}
    </div >
  );
};
