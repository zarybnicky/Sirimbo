import * as React from 'react';
import { History, createBrowserHistory } from 'history';
import { ConnectedRouter, routerMiddleware, connectRouter } from 'connected-react-router';
import { applyMiddleware, combineReducers, createStore } from 'redux';
import { Provider } from 'react-redux';
import createSagaMiddleware from 'redux-saga';
import { all, fork } from 'redux-saga/effects';
import { Redirect, Switch, Route, useLocation } from 'react-router-dom';

import { ThemeProvider, makeStyles } from '@material-ui/styles';
import { createTheme } from '@material-ui/core/styles';
import { CssBaseline, AppBar, Container, Link, Toolbar, Typography } from '@material-ui/core';

import { adminReducer, adminSaga, DataProvider, DataProviderContext, Resource } from 'ra-core';
import { ListGuesser, EditGuesser, ShowGuesser, Notification } from 'ra-ui-materialui';

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

const theme = createTheme({
  palette: {
    primary: {
      main: "#f60000",
    },
    secondary: {
      main: '#000',
    },
  },
});

const Home = () => <div>Home</div>
const ArticleList = () => <div>ArticleList</div>
const ArticleShow = () => {
  return <div>ArticleShow</div>;
};

const DynamicRoute = () => {
  let location = useLocation();
  return <React.Fragment>location.pathname</React.Fragment>;
}

const routes = <Switch>
  <Route exact path="/"><Home /></Route>

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

  <Route exact path="/news"><ArticleList /></Route>
  <Route exact path="/news/:id"><ArticleShow /></Route>

  {/* Galerie - foto, video */}

  {/* Akce - soutěže, soustředění s jednoduchým přihlašovacím systémem, plesy a akce s možnosti rezervace vstupenek */}

  {/* Kontakt - fakturační údaje, kontakty na jednotlivé činovníky, sekretáře, vedoucí poboček */}

  <Redirect from="/home" to="/" />
  <Redirect from="/aktualne" to="/news" />
  <Redirect from="/aktualne/:id" to="/news/:id" />

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

const useStyles = makeStyles((theme) => ({
  navbar: {
    justifyContent: 'space-between',
    display: "flex",
  },
  logo: {
    flexGrow: 1,
    cursor: "pointer",
  },
  link: {
    textDecoration: "none",
    color: "white",
    fontSize: "20px",
    "&:hover": {
      color: "yellow",
      borderBottom: "1px solid white",
    },
  },
}));

const AppHeader = () => {
  const classes = useStyles();
  return <AppBar position="static" color="secondary">
    <Toolbar>
      <Container maxWidth="lg" className={classes.navbar}>
        <Typography variant="h6" color="inherit">Admin</Typography>
      </Container>
    </Toolbar>
  </AppBar>;
};

const history = createBrowserHistory({ basename: '/app' });
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
        <ConnectedRouter history={history}>
          <CssBaseline />
          <Resource name="upozorneni" intent="registration" />
          <AppHeader />
          {routes}
          <Notification />
        </ConnectedRouter>
      </ThemeProvider>
    </DataProviderContext.Provider>
  </Provider>;
}
