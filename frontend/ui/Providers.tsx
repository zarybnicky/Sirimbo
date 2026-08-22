'use client';

import 'core-js/actual/array/to-reversed';
import 'core-js/actual/array/to-sorted';

import { configureUrql } from '@/lib/query';
import type { TenantCatalogEntry } from '@/tenant/catalog';
import { ConfirmProvider } from '@/ui/Confirm';
import { ErrorNotifier } from '@/ui/ErrorNotifier';
import { FillYourProfileReminder } from '@/ui/FillYourProfileReminder';
import { authAtom, tenantAtom, storeRef, type RequestAuthState } from '@/lib/auth';
import { parseUiState, sidebarWidthAtom, uiAtom } from '@/lib/ui';
import { Tracking } from '@/ui/Tracking';
import { UpdateNotifier } from '@/ui/UpdateNotifier';
import { createStore, Provider as JotaiProvider, useAtomValue } from 'jotai';
import React from 'react';
import { ToastContainer } from 'react-toastify';
import { createClient, Provider as UrqlProvider } from 'urql';
import { SessionRefresher } from './SessionRefresher';

export function Providers({
  children,
  initialAuth,
  initialTenant,
  initialUiCookie,
}: {
  children: React.ReactNode;
  initialAuth: RequestAuthState;
  initialTenant: TenantCatalogEntry;
  initialUiCookie?: string;
}) {
  const [store] = React.useState(() => {
    const store = createStore();
    store.set(authAtom, initialAuth);
    store.set(tenantAtom, initialTenant);
    store.set(uiAtom, parseUiState(initialUiCookie));
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
          <UiStateStyles>{children}</UiStateStyles>
          <UpdateNotifier />
          <FillYourProfileReminder />
          <ErrorNotifier />
          <SessionRefresher />
          <ToastContainer limit={3} />
        </ConfirmProvider>
      </UrqlProvider>
    </JotaiProvider>
  );
}

function UiStateStyles({ children }: { children: React.ReactNode }) {
  const sidebarWidth = useAtomValue(sidebarWidthAtom);

  return (
    <div
      className="contents"
      style={
        sidebarWidth === null
          ? undefined
          : ({ '--sidebar-width': `${sidebarWidth}px` } as React.CSSProperties)
      }
    >
      {children}
    </div>
  );
}
