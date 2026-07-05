'use client';

import 'core-js/actual/array/to-reversed';
import 'core-js/actual/array/to-sorted';

import { configureUrql } from '@/lib/query';
import { hostToTenantId } from '@/tenant/catalog-server';
import { ConfirmProvider } from '@/ui/Confirm';
import { ErrorNotifier } from '@/ui/ErrorNotifier';
import { FillYourProfileReminder } from '@/ui/FillYourProfileReminder';
import { setNewTenant, storeRef } from '@/ui/state/auth';
import { Tracking } from '@/ui/Tracking';
import { UpdateNotifier } from '@/ui/UpdateNotifier';
import { UserRefresher } from '@/ui/use-auth';
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { getCookie, setCookie } from 'cookies-next/client';
import { createStore, Provider as JotaiProvider } from 'jotai';
import React from 'react';
import { ToastContainer } from 'react-toastify';
import { createClient, Provider as UrqlProvider } from 'urql';

export function Providers({
  children,
  tenantId,
}: {
  children: React.ReactNode;
  tenantId: string;
}) {
  const [store] = React.useState(() => createStore());
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
          <TenantCookieSync fallbackTenantId={tenantId} />
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

function TenantCookieSync({ fallbackTenantId }: { fallbackTenantId: string }) {
  useLayoutEffect(() => {
    const origin = new URL(window.origin);
    const tenantId = hostToTenantId.get(origin.hostname) ?? fallbackTenantId;
    const existing = String(getCookie('tenant_id'));
    const domain =
      origin.hostname === 'localhost' || origin.hostname === '127.0.0.1'
        ? undefined
        : origin.hostname;

    if (tenantId !== existing) {
      setCookie('tenant_id', tenantId, {
        path: '/',
        sameSite: 'lax',
        secure: origin.protocol === 'https:',
        domain,
        expires: new Date(Date.now() + 1000 * 60 * 60 * 24 * 365 * 10),
      });
    }
    setNewTenant(tenantId);
  }, [fallbackTenantId]);

  return null;
}
