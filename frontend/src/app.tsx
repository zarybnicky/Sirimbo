import * as React from 'react';
import { ApolloProvider } from '@apollo/client';
import { createBrowserHistory } from 'history';
import { ConnectedRouter } from 'connected-react-router';
import { Provider } from 'react-redux';
import { ThemeProvider } from '@material-ui/styles';

import { DataProvider, DataProviderContext, Resource } from 'ra-core';

import { Layout } from './components/Layout';
import { createClient } from './client';
import { ProvideAuth } from './use-auth';
import { routes } from './routes';
import { createAppStore } from './store';
import { theme } from './theme';

const history = createBrowserHistory({ basename: '/app' });
const client = createClient();

export const App = () => {
  /* const [dataProvider, setDataProvider] = React.useState<DataProvider | null>(null);
   * React.useEffect(() => {
   *   (async () => {
   *     setDataProvider(await buildHasuraProvider('/graphql', {}, {
   *       primaryKey: {
   *         'upozorneni': 'up_id',
   *       },
   *     }));
   *   })();
   * }, []);
   * if (!dataProvider) {
   *   return null;
   * } */

  return <Provider store={createAppStore({} as DataProvider, history)}>
    <DataProviderContext.Provider value={null as any}>
      <ThemeProvider theme={theme}>
        <ApolloProvider client={client}>
          <ConnectedRouter history={history}>
            <ProvideAuth>
              <Resource name="upozorneni" intent="registration" />
              <Layout>{routes}</Layout>
            </ProvideAuth>
          </ConnectedRouter>
        </ApolloProvider>
      </ThemeProvider>
    </DataProviderContext.Provider>
  </Provider>;
}
