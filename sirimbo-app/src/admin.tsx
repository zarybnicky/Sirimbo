import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState, useEffect } from 'react';
import buildHasuraProvider from 'ra-data-hasura';

import { Provider } from 'react-redux';
import { History, createHashHistory } from 'history';
import { ConnectedRouter } from 'connected-react-router';
import { Switch, Route } from 'react-router-dom';
import { DataProvider, DataProviderContext, TranslationProvider, Resource, Notification } from 'react-admin';
import defaultMessages from 'ra-language-english';
import polyglotI18nProvider from 'ra-i18n-polyglot';
import { ThemeProvider } from '@material-ui/styles';
import { createTheme } from "@material-ui/core/styles";
import AppBar from '@material-ui/core/AppBar';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';

import createAdminStore from './createAdminStore';

/* import { UpozorneniList, UpozorneniCreate, UpozorneniEdit, UpozorneniShow } from './UpozorneniAdmin'; */
import { ListGuesser, EditGuesser, ShowGuesser } from 'react-admin';
const Dashboard = () => <div>Dashboard</div>;

const i18nProvider = polyglotI18nProvider(locale => {
    /* if (locale !== 'en') {
     *     return messages[locale];
     * } */
    return defaultMessages;
});
const theme = createTheme();

export const AdminPanel = () => {
    const [history, setHistory] = useState<History<unknown> | null>(null);
    const [dataProvider, setDataProvider] = useState<DataProvider | null>(null);
    useEffect(() => {
        (async () => {
            setHistory(createHashHistory());
            const dataProvider = await buildHasuraProvider({
                clientOptions: {
                    uri: '/graphql/v1/graphql',
                },
            });
            setDataProvider({
                getList: (res, params) => dataProvider("GET_LIST", res, params),
                getOne: (res, params) => dataProvider("GET_ONE", res, params),
                getMany: (res, params) => dataProvider("GET_MANY", res, params),
                getManyReference: (res, params) => dataProvider("GET_MANY_REFERENCE", res, params),
                update: (res, params) => dataProvider("UPDATE", res, params),
                updateMany: (res, params) => dataProvider("UPDATE_MANY", res, params),
                create: (res, params) => dataProvider("CREATE", res, params),
                delete: (res, params) => dataProvider("DELETE", res, params),
                deleteMany: (res, params) => dataProvider("DELETE_MANY", res, params),
            });
        })()
    }, []);
    if (!dataProvider || !history) {
        return null;
    }

    return <Provider store={createAdminStore({ dataProvider, history })}>
        <DataProviderContext.Provider value={dataProvider}>
            <TranslationProvider locale="en" i18nProvider={i18nProvider}>
                <ThemeProvider theme={theme}>
                    <Resource name="upozorneni" intent="registration" />
                    <AppBar position="static" color="default">
                        <Toolbar>
                            <Typography variant="h6" color="inherit">Admin</Typography>
                        </Toolbar>
                    </AppBar>
                    <ConnectedRouter history={history}>
                        <Switch>
                            <Route exact path="/admin" component={Dashboard} />
                            <Route exact path="/admin/upozorneni" render={(routeProps) => <ListGuesser hasCreate resource="upozorneni" basePath={routeProps.match.url} {...routeProps} />} />
                            {/* <Route exact path="/admin/upozorneni/create" render={(routeProps) => <UpozorneniCreate resource="upozorneni" basePath={routeProps.match.url} {...routeProps} />} /> */}
                            <Route exact path="/admin/upozorneni/:id" render={(routeProps) => <EditGuesser hasShow resource="upozorneni" basePath={routeProps.match.url} id={decodeURIComponent((routeProps.match).params.id)} {...routeProps} />} />
                            <Route exact path="/admin/upozorneni/:id/show" render={(routeProps) => <ShowGuesser hasEdit resource="upozorneni" basePath={routeProps.match.url} id={decodeURIComponent((routeProps.match).params.id)} {...routeProps} />} />
                        </Switch>
                    </ConnectedRouter>
                    <Notification />
                </ThemeProvider>
            </TranslationProvider>
        </DataProviderContext.Provider>
    </Provider>;
}

class AdminPanelElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(<AdminPanel />, this);
    }
}
customElements.define('admin-panel', AdminPanelElement);
