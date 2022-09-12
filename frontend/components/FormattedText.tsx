import Image from "next/image"
import parse, { HTMLReactParserOptions, domToReact, DOMNode, Element } from "html-react-parser"
import { NextLinkComposed, isRelative } from "./Link"

const isElement = (domNode: DOMNode): domNode is Element => {
  const isTag = domNode.type === "tag";
  const hasAttributes = (domNode as Element).attribs !== undefined;

  return isTag && hasAttributes;
};

const options: HTMLReactParserOptions = {
  replace: (domNode) => {
    if (isElement(domNode)) {
      if (domNode.name === "img") {
        const { src, alt, width = "100px", height = "100px" } = domNode.attribs
        if (src && isRelative(src)) {
          return (
            <Image
              src={src}
              width={`${width}px`}
              height={`${height}px`}
              alt={alt}
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
    }
  },
}

interface FormattedTextProps extends React.HTMLAttributes<HTMLDivElement> {
  format?: string
  processed: string
  value?: string
}

export function FormattedText({ processed, className, ...props }: FormattedTextProps) {
  return (
    <div className={`prose ${className}`} {...props} style={{
      overflowWrap: "break-word",
      wordWrap: "break-word",
      wordBreak: "break-word",
    }}>
      {parse(processed, options)}
    </div>
  )
}
