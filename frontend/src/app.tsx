import * as React from 'react';
import { ApolloProvider, HttpLink, ApolloClient, InMemoryCache } from '@apollo/client';
import { BrowserRouter } from 'react-router-dom';
import { ThemeProvider } from '@material-ui/styles';
import { SnackbarProvider } from 'notistack';
import { ConfirmProvider } from 'material-ui-confirm';

import { Layout } from './components/Layout';
import { ProvideAuth } from './data/use-auth';
import { routes } from './routes';
import { theme } from './theme';

const client = new ApolloClient({
  link: new HttpLink({ uri: '/graphql' }),
  cache: new InMemoryCache(),
});

export const App = () => {
  return <ThemeProvider theme={theme}>
    <SnackbarProvider>
      <ConfirmProvider defaultOptions={{
        confirmationButtonProps: { autoFocus: true }
      }}>
        <ApolloProvider client={client}>
          <BrowserRouter basename="/app">
            <ProvideAuth>
              <Layout>{routes}</Layout>
            </ProvideAuth>
          </BrowserRouter>
        </ApolloProvider>
      </ConfirmProvider>
    </SnackbarProvider>
  </ThemeProvider>;
}
