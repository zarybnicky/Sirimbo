import * as React from 'react';
import { theme } from './theme';
import { ApolloProvider } from '@apollo/client';
import { History, createBrowserHistory } from 'history';
import { ConnectedRouter, routerMiddleware, connectRouter } from 'connected-react-router';
import { applyMiddleware, combineReducers, createStore } from 'redux';
import { Provider } from 'react-redux';
import createSagaMiddleware from 'redux-saga';
import { all, fork } from 'redux-saga/effects';
import { Redirect, Switch, Route, useLocation } from 'react-router-dom';

import { ThemeProvider } from '@material-ui/styles';

import { adminReducer, adminSaga, DataProvider, DataProviderContext, Resource } from 'ra-core';
import { ListGuesser, EditGuesser, ShowGuesser } from 'ra-ui-materialui';

import { createClient } from './client';

import { Layout } from './components/Layout';
import { PageEditor } from './pages/PageEditor';
import { HomePage } from './pages/HomePage';
import { ProvideAuth } from './use-auth';

const createAppStore = (dataProvider: DataProvider, history: History) => {
  const reducer = combineReducers({
    admin: adminReducer,
    router: connectRouter(history),
  });
  const saga = function* rootSaga() {
    yield all([adminSaga(dataProvider, null)].map(fork));
  };
  const sagaMiddleware = createSagaMiddleware();
  const store = createStore(reducer, applyMiddleware(sagaMiddleware, routerMiddleware(history)));
  sagaMiddleware.run(saga);
  return store;
};

const DynamicRoute = () => {
  let location = useLocation();
  return <React.Fragment>{location.pathname}</React.Fragment>;
}

const routes = <Switch>
  <Redirect exact from="/" to="/home" />
  <Route exact path="/home"><HomePage /></Route>

  <Redirect exact from="/aktualne" to="/news" />
  <Redirect exact from="/aktualne/:id" to="/news/:id" />
  <Route exact path="/news">
    Article list
  </Route>
  <Route exact path="/news/:id">
    Show article
  </Route>
  <Route exact path="/news/:id/edit">
    <PageEditor />
  </Route>

  {/* Klub: dynamic rendering
      - O nás
      - Kde trénujeme - mapky, adresy, popis vstupu
      - Tréninkové skupiny - dohromady kluboví, externí
      - Benefity členství - slevy na soustředění, na oblečení, obuv, popis péče o členy
      - Galerie mistrů: foto – poháry (FOTKY – prolnout po  vteřinách) + popisek (fotka na banner)
      - "Chci tančit" - cílová stránka
    */}
  {/* Nabízíme
      - Tréninkové programy (Popis tanečního sportu a možnosti + odkaz na tréninkové skupiny)
      - Školní taneční kroužky (statický popis plus odkaz na www.olympdance.cz)
      - Vystoupení na akcích, poptávkový formulář
    */}
  {/* Galerie - foto, video */}
  {/* Akce - soutěže, soustředění s jednoduchým přihlašovacím systémem, plesy a akce s možnosti rezervace vstupenek */}
  {/* Kontakt - fakturační údaje, kontakty na jednotlivé činovníky, sekretáře, vedoucí poboček */}

  <Route exact path="/admin/upozorneni" render={(routeProps) =>
    <ListGuesser hasCreate resource="upozorneni"
      basePath={routeProps.match.url} {...routeProps} />} />
  <Route exact path="/admin/upozorneni/:id" render={(routeProps) =>
    <EditGuesser hasShow resource="upozorneni"
      basePath={routeProps.match.url} id={routeProps.match.params.id} {...routeProps} />} />
  <Route exact path="/admin/upozorneni/:id/show" render={(routeProps) =>
    <ShowGuesser hasEdit resource="upozorneni"
      basePath={routeProps.match.url} id={routeProps.match.params.id} {...routeProps} />} />

  <Route><DynamicRoute /></Route>
</Switch>;

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