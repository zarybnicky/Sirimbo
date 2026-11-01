'use client';

import { TenantSettingsDocument } from '@/graphql/Tenant';
import { useTenantId } from '@/lib/auth';
import { StarletImporter } from '@/ui/starlet-importer';
import { starletSettingsAtom, starletTokenAtom } from '@/ui/starlet-importer/state';
import { useAtom, useSetAtom } from 'jotai';
import { useEffect } from 'react';
import { useQuery } from 'urql';

export function StarletImport() {
  const tenantId = useTenantId();
  const [{ data: settingsQuery }] = useQuery({
    query: TenantSettingsDocument,
    variables: { tenantId },
  });
  const logIn = useSetAtom(starletTokenAtom);
  const [{ auth }, setSettings] = useAtom(starletSettingsAtom);

  useEffect(() => {
    setSettings(settingsQuery?.tenantSetting?.settings || '{}');
  }, [setSettings, settingsQuery]);

  useEffect(() => {
    logIn(auth?.login, auth?.password);
  }, [auth?.login, auth?.password, logIn]);

  return <StarletImporter />;
}
