import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { ReactPage } from '../components/ReactPage';

import { ApolloProvider, HttpLink, ApolloClient, InMemoryCache } from '@apollo/client';
import { SnackbarProvider } from 'notistack';
import { ThemeProvider } from '@material-ui/styles';
import { theme } from '../theme';

import '@react-page/editor/lib/index.css';
import '@react-page/plugins-slate/lib/index.css';
import '@react-page/plugins-image/lib/index.css';

const client = new ApolloClient({
  link: new HttpLink({ uri: '/graphql' }),
  cache: new InMemoryCache(),
});

export class ReactPageElement extends HTMLElement {
  connectedCallback() {
    let page = this.getAttribute('page') || '{}';
    page = page.replace('\"', '"');
    page = JSON.parse(page);
    ReactDOM.render((
      <ApolloProvider client={client}>
        <ThemeProvider theme={theme}>
          <SnackbarProvider maxSnack={3} anchorOrigin={{ horizontal: 'right', vertical: 'bottom' }}>
            <ReactPage readOnly value={JSON.parse(page)} />
          </SnackbarProvider>
        </ThemeProvider>
      </ApolloProvider>
    ), this);
  }
}
