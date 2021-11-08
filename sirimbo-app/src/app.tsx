import * as React from 'react';
import { useState, useEffect } from 'react';

import { combineReducers, createStore } from 'redux';
import { Provider } from 'react-redux';
import { Switch, Route } from 'react-router-dom';
import { adminReducer, DataProvider, DataProviderContext, Resource } from 'ra-core';

import { ThemeProvider, createTheme } from '@mui/material/styles';
import { red } from "@mui/material/colors";
import { CssBaseline, AppBar, Toolbar, Typography } from '@mui/material';

import { ListGuesser, EditGuesser, ShowGuesser, Notification } from 'ra-ui-materialui';

const createAppStore = () => {
    const reducer = combineReducers({ admin: adminReducer, });
    const resettableAppReducer = (state: any, action: any) =>
        reducer(action.type !== 'LOGOUT' ? state : undefined, action);
    return createStore(resettableAppReducer);
};

const theme = createTheme({
    palette: {
        mode: 'dark',
        primary: red,
    },
});

export const App = () => {
    const [dataProvider, setDataProvider] = useState<DataProvider | null>(null);
    useEffect(() => {
        (async () => {
            /* const dataProvider = await buildHasuraProvider({
             *     clientOptions: {
             *         uri: '/graphql/v1/graphql',
             *     },
             * });
             * setDataProvider({
             *     getList: (res, params) => dataProvider("GET_LIST", res, params),
             *     getOne: (res, params) => dataProvider("GET_ONE", res, params),
             *     getMany: (res, params) => dataProvider("GET_MANY", res, params),
             *     getManyReference: (res, params) => dataProvider("GET_MANY_REFERENCE", res, params),
             *     update: (res, params) => dataProvider("UPDATE", res, params),
             *     updateMany: (res, params) => dataProvider("UPDATE_MANY", res, params),
             *     create: (res, params) => dataProvider("CREATE", res, params),
             *     delete: (res, params) => dataProvider("DELETE", res, params),
             *     deleteMany: (res, params) => dataProvider("DELETE_MANY", res, params),
             * }); */
        })()
    }, []);
    if (!dataProvider) {
        return null;
    }

    const router = <Switch>
        <Route exact path="/" component={() => <div>Test</div>} />
        <Route exact path="/admin/upozorneni" render={(routeProps) =>
            <ListGuesser hasCreate resource="upozorneni" basePath={routeProps.match.url} {...routeProps} />} />
        <Route exact path="/admin/upozorneni/:id" render={(routeProps) =>
            <EditGuesser hasShow resource="upozorneni" basePath={routeProps.match.url} id={routeProps.match.params.id} {...routeProps} />} />
        <Route exact path="/admin/upozorneni/:id/show" render={(routeProps) =>
            <ShowGuesser hasEdit resource="upozorneni" basePath={routeProps.match.url} id={routeProps.match.params.id} {...routeProps} />} />
    </Switch>;

    return <Provider store={createAppStore()}>
        <DataProviderContext.Provider value={dataProvider}>
            <ThemeProvider theme={theme}>
                <CssBaseline />
                <Resource name="upozorneni" intent="registration" />
                <AppBar position="static" color="default">
                    <Toolbar>
                        <Typography variant="h6" color="inherit">Admin</Typography>
                    </Toolbar>
                </AppBar>
                {router}
                <Notification />
            </ThemeProvider>
        </DataProviderContext.Provider>
    </Provider>;
}
