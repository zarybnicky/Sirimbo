import { Container, Typography } from '@mui/material';
import { NextLinkComposed } from "./Link"
import parse, { domToReact, DOMNode, Element, HTMLReactParserOptions } from "html-react-parser"

const isElement = (domNode: DOMNode): domNode is Element => {
  const isTag = domNode.type === "tag";
  const hasAttributes = (domNode as Element).attribs !== undefined;

  return isTag && hasAttributes;
};

const options: HTMLReactParserOptions = {
  replace: (domNode) => {
    if (isElement(domNode)) {
      if (domNode.name === "a") {
        const { href, class: className, ...rest } = domNode.attribs
        return (
          <NextLinkComposed href={href || '#'} className={className} {...rest}>
            {domToReact(domNode.children as DOMNode[])}
          </NextLinkComposed>
        );
      }
    }
  },
};

export const HtmlView: React.FC<{
  title?: string;
  subheader?: string;
  content: string;
}> = (page) => {
  return (
    <Container maxWidth="lg" sx={{ margin: '80px auto' }}>
      {page.title && <Typography variant="h3" component="h2">{page.title}</Typography>}
      {page.subheader && <div dangerouslySetInnerHTML={{ __html: page.subheader }} />}

      <div className='prose' style={{
        overflowWrap: "break-word",
        wordWrap: "break-word",
        wordBreak: "break-word",
      }}>
        {parse(page.content, options)}
      </div >
    </Container>
  );

};
