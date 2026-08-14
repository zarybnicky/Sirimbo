'use client';

import 'core-js/actual/array/to-reversed';
import 'core-js/actual/array/to-sorted';

import { configureUrql } from '@/lib/query';
import type { TenantCatalogEntry } from '@/tenant/catalog';
import { ConfirmProvider } from '@/ui/Confirm';
import { ErrorNotifier } from '@/ui/ErrorNotifier';
import { FillYourProfileReminder } from '@/ui/FillYourProfileReminder';
import {
  requestAuthAtom,
  requestTenantAtom,
  storeRef,
  type RequestAuthState,
} from '@/ui/state/auth';
import { Tracking } from '@/ui/Tracking';
import { UpdateNotifier } from '@/ui/UpdateNotifier';
import { UserRefresher } from '@/ui/use-auth';
import { createStore, Provider as JotaiProvider } from 'jotai';
import React from 'react';
import { ToastContainer } from 'react-toastify';
import { createClient, Provider as UrqlProvider } from 'urql';

export function Providers({
  children,
  initialAuth,
  initialTenant,
}: {
  children: React.ReactNode;
  initialAuth: RequestAuthState;
  initialTenant: TenantCatalogEntry;
}) {
  const [store] = React.useState(() => {
    const store = createStore();
    store.set(requestAuthAtom, initialAuth);
    store.set(requestTenantAtom, initialTenant);
    return store;
  });
  const [client, setClient] = React.useState(() => createClient(configureUrql()));
  const resetUrqlClient = React.useCallback(() => {
    setClient(createClient(configureUrql()));
  }, []);

  // eslint-disable-next-line react-hooks/immutability
  storeRef.current = store;
  // eslint-disable-next-line react-hooks/immutability
  storeRef.resetUrqlClient = resetUrqlClient;

  return (
    <JotaiProvider store={store}>
      <UrqlProvider value={client}>
        <ConfirmProvider>
          <Tracking />
          {children}
          <UpdateNotifier />
          <FillYourProfileReminder />
          <ErrorNotifier />
          <UserRefresher />
          <ToastContainer limit={3} />
        </ConfirmProvider>
      </UrqlProvider>
    </JotaiProvider>
  );
}
