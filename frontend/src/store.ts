import { applyMiddleware, combineReducers, createStore } from 'redux';
import { adminReducer, adminSaga, DataProvider } from 'ra-core';
import { routerMiddleware, connectRouter } from 'connected-react-router';
import { History } from 'history';
import createSagaMiddleware from 'redux-saga';
import { all, fork } from 'redux-saga/effects';

export const createAppStore = (dataProvider: DataProvider, history: History) => {
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
