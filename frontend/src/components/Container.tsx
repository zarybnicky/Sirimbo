import * as React from 'react';
import { Container } from '@material-ui/core';
import type { CellPlugin } from '@react-page/editor';

export const ContainerPlugin: CellPlugin<{
  maxWidth: 'sm' | 'md' | 'lg';
}> = {
  Renderer: ({ children, data }) => (
    <Container maxWidth={data.maxWidth} style={{ padding: '2rem .5rem' }}>
      <React.Fragment>{children}</React.Fragment>
    </Container>
  ),

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