import React from 'react';
import { MemoryRouter } from 'react-router';
import { CssBaseline } from '@material-ui/core';
import { ThemeProvider } from '@material-ui/styles';
import { ProvideAuth } from './use-auth';
import { theme } from './theme';

export const StoryTemplate = ({ children }: { children: JSX.Element | JSX.Element[] }) => (
  <ThemeProvider theme={theme}>
    <CssBaseline />
    <ProvideAuth mock>
      <MemoryRouter>
        {children}
      </MemoryRouter>
    </ProvideAuth>
  </ThemeProvider>
);
