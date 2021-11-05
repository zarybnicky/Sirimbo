import { applyMiddleware, combineReducers, compose, createStore } from 'redux';
import { routerMiddleware, connectRouter } from 'connected-react-router';
import createSagaMiddleware from 'redux-saga';
import { all, fork } from 'redux-saga/effects';
import {
  adminReducer,
  adminSaga,
  USER_LOGOUT,
} from 'react-admin';

export default ({ dataProvider, history }) => {
  const reducer = combineReducers({
    admin: adminReducer,
    router: connectRouter(history),
    // add your own reducers here
  });
  const resettableAppReducer = (state, action) =>
    reducer(action.type !== USER_LOGOUT ? state : undefined, action);

  const saga = function* rootSaga() {
    yield all(
      [
        adminSaga(dataProvider, null),
        // add your own sagas here
      ].map(fork)
    );
  };
  const sagaMiddleware = createSagaMiddleware();

  const store = createStore(
    resettableAppReducer,
    { /* set your initial state here */ },
    compose(
      applyMiddleware(
        sagaMiddleware,
        routerMiddleware(history),
        // add your own middlewares here
      ),
      // add your own enhancers here
    ),
  );
  sagaMiddleware.run(saga);
  return store;
};
