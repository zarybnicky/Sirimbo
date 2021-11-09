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
  holder?: string
  children?: JSX.Element | JSX.Element[]
  value?: EditorConfig['data']
  defaultValue?: EditorConfig['data']

  onInitialize?: (editorJS: EditorJS) => void
}

export { EditorJS };

export function ReactEditorJS({
  holder,
  tools,
  defaultValue,
  children,
  value,
  onInitialize,
  ...restProps
}: Props) {
  const memoizedHolder = React.useRef(
    holder ?? `react-editor-js-${Date.now().toString(16)}`
  )

  const editorJS = React.useRef<EditorJS | null>(null)

  React.useEffect(() => {
    const extendTools = {
      // default tools
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

      ...tools,
    }

    editorJS.current = new EditorJS({
      tools: extendTools,
      holder: memoizedHolder.current,
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

  return <React.Fragment>children</React.Fragment> || <div id={memoizedHolder.current} />
}
