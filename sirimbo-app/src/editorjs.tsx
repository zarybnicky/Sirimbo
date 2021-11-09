import * as React from 'react';
import EditorJS, { EditorConfig } from '@editorjs/editorjs';

import Paragraph from '@editorjs/paragraph';
import Embed from '@editorjs/embed';
import Table from '@editorjs/table';
import List from '@editorjs/list';
import Warning from '@editorjs/warning';
import LinkTool from '@editorjs/link';
import Image from '@editorjs/image';
import Header from '@editorjs/header';
import Quote from '@editorjs/quote';
import Delimiter from '@editorjs/delimiter';
import SimpleImage from '@editorjs/simple-image';

interface Props extends Omit<EditorConfig, 'data'> {
  value?: EditorConfig['data']
  defaultValue?: EditorConfig['data']

  onInitialize?: (editorJS: EditorJS) => void
}

const defaultTools = {
  paragraph: {
    class: Paragraph,
    inlineToolbar: true,
  },
  embed: Embed,
  table: Table,
  list: List,
  warning: Warning,
  linkTool: LinkTool,
  image: Image,
  header: Header,
  quote: Quote,
  delimiter: Delimiter,
  simpleImage: SimpleImage,
};

export function ReactEditorJS({
  tools,
  defaultValue,
  value,
  onInitialize,
  ...restProps
}: Props) {
  const editorJS = React.useRef<EditorJS | null>(null)

  const holder = React.useCallback((node) => {
    if (node === null) {
      return;
    }
    const extendTools = { ...defaultTools, ...tools };

    editorJS.current = new EditorJS({
      tools: extendTools,
      holder: node,
      ...(defaultValue && { data: defaultValue }),
      ...restProps,
    })
    onInitialize?.(editorJS.current)
    return () => {
      editorJS.current?.destroy()
    }
  }, [])

  React.useEffect(() => {
    if (value) {
      editorJS.current?.blocks.render(value)
    }
  }, [value])

  return <div ref={holder} />
}
