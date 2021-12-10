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
/* import { ListGuesser, EditGuesser, ShowGuesser } from 'ra-ui-materialui'; */

import { createClient } from './client';
import { ProvideAuth } from './use-auth';

import { Layout } from './components/Layout';
import { PageEditor } from './pages/PageEditor';
import { AboutPage } from './pages/AboutPage';
import { HomePage } from './pages/HomePage';
import { LocationsPage } from './pages/LocationsPage';
import { NewsPage } from './pages/NewsPage';
import { TrainersPage } from './pages/TrainersPage';
import { GalleryPage } from './pages/GalleryPage';

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
  <Route exact path="/o-nas"><AboutPage /></Route>
  <Route exact path="/o-nas/kde-trenujeme"><LocationsPage /></Route>
  <Route exact path="/o-nas/treneri"><TrainersPage /></Route>
  {/* <Route exact path="/o-nas/treninkove-skupiny"><CohortsPage /></Route>
      <Route exact path="/o-nas/clenstvi"><MembershipPage /></Route>
      <Route exact path="/o-nas/galerie-mistru"><HallOfFamePage /></Route> */}

  <Redirect exact from="/nabizime" to="/nabizime/treninkove-programy" />
  {/* <Route exact path="/nabizime/treninkove-programy"><TrainingOfferPage /></Route>
      <Route exact path="/nabizime/skolni-krouzky"><SchoolOfferPage /></Route>
      <Route exact path="/nabizime/vystoupeni"><ShowOfferPage /></Route> */}

  <Route exact path="/aktualne"><NewsPage /></Route>
  <Route exact path="/aktualne/:id">Show article</Route>
  <Route exact path="/aktualne/:id/edit"><PageEditor /></Route>

  <Route exact path="/galerie"><GalleryPage /></Route>

  {/* <Route exact path="/admin/upozorneni" render={(routeProps) =>
      <ListGuesser hasCreate resource="upozorneni"
      basePath={routeProps.match.url} {...routeProps} />} />
      <Route exact path="/admin/upozorneni/:id" render={(routeProps) =>
      <EditGuesser hasShow resource="upozorneni"
      basePath={routeProps.match.url} id={routeProps.match.params.id} {...routeProps} />} />
      <Route exact path="/admin/upozorneni/:id/show" render={(routeProps) =>
      <ShowGuesser hasEdit resource="upozorneni"
      basePath={routeProps.match.url} id={routeProps.match.params.id} {...routeProps} />} /> */}

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
