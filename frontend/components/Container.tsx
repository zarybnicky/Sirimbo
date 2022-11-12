import * as React from 'react';
import type { CellPlugin } from '@react-page/editor';

export const ContainerPlugin: CellPlugin<{
  maxWidth: 'sm' | 'md' | 'lg';
}> = {
  Renderer: ({ children, data }) => {
    const width = data.maxWidth === 'lg' ? 'max-w-5xl' :
      data.maxWidth === 'md' ? 'max-w-3xl' : 'max-w-xl';
    return <div className={`${width} px-2 py-8`}>{children}</div>;
  },

  id: 'app-container-plugin',
  title: 'Container',
  description: undefined,
  version: 1,
  cellSpacing: 20,
  createInitialData: () => ({
    maxWidth: 'lg',
  }),
  controls: {
    type: 'autoform',
    schema: {
      required: ['maxWidth'],
      properties: {
        maxWidth: {
          type: 'string',
          enum: ['sm', 'md', 'lg'],
        },
      },
    },
  },
};
