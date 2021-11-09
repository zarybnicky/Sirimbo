import * as React from 'react';
import { combineReducers, createStore } from 'redux';
import { Provider } from 'react-redux';
import { BrowserRouter, Redirect, Switch, Route, useLocation } from 'react-router-dom';
import { adminReducer, DataProviderContext, Resource } from 'ra-core';

import { ThemeProvider, createTheme } from '@mui/material/styles';
import { red } from "@mui/material/colors";
import {
  CssBaseline, AppBar, Container, Link, Toolbar, Typography,
} from '@material-ui/core';

import { ListGuesser, EditGuesser, ShowGuesser, Notification } from 'ra-ui-materialui';

const createAppStore = () => {
  const reducer = combineReducers({ admin: adminReducer, });
  const resettableAppReducer = (state: any, action: any) =>
    reducer(action.type !== 'LOGOUT' ? state : undefined, action);
  return createStore(resettableAppReducer);
};

const theme = createTheme({
  palette: {
    primary: red,
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

const AppHeader = () => <AppBar position="static" color="secondary">
  <Toolbar>
    <Container maxWidth="lg" sx={{ display: `flex`, justifyContent: `space-between` }}>
      <Typography variant="h6" color="inherit">Admin</Typography>
    </Container>
  </Toolbar>
</AppBar>;


export const App = () => {
  /* const [dataProvider, setDataProvider] = useState<DataProvider | null>(null);
   * useEffect(() => {
   * (async () => {
   *         const dataProvider = await buildHasuraProvider({
      *             clientOptions: {
   *                 uri: '/graphql/v1/graphql',
      *             },
   *         });
    *         setDataProvider({
   * getList: (res, params) => dataProvider("GET_LIST", res, params),
   *             getOne: (res, params) => dataProvider("GET_ONE", res, params),
   *             getMany: (res, params) => dataProvider("GET_MANY", res, params),
   *             getManyReference: (res, params) => dataProvider("GET_MANY_REFERENCE", res, params),
   *             update: (res, params) => dataProvider("UPDATE", res, params),
   *             updateMany: (res, params) => dataProvider("UPDATE_MANY", res, params),
   *             create: (res, params) => dataProvider("CREATE", res, params),
   *             delete: (res, params) => dataProvider("DELETE", res, params),
   *             deleteMany: (res, params) => dataProvider("DELETE_MANY", res, params),
   *         });
   *     })()
   * }, []);
    * if (!dataProvider) {
   *     return null;
   * } */

  return <Provider store={createAppStore()}>
    <DataProviderContext.Provider value={null as any}>
      <ThemeProvider theme={theme}>
        <BrowserRouter basename='/app'>
          <CssBaseline />
          <Resource name="upozorneni" intent="registration" />
          <AppHeader />
          {routes}
          <Notification />
        </BrowserRouter>
      </ThemeProvider>
    </DataProviderContext.Provider>
  </Provider>;
}
