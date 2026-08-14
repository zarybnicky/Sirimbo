'use client';

import { TenantSettingsDocument } from '@/graphql/CurrentUser';
import { useEffect } from 'react';
import { useQuery } from 'urql';
import { starletSettingsAtom, starletTokenAtom } from '@/ui/starlet-importer/state';
import { useAtom } from 'jotai';
import { StarletImporter } from '@/ui/starlet-importer';
import { useTenantId } from '@/ui/state/auth';

export function StarletImport() {
  const tenantId = useTenantId();
  const [{ data: settingsQuery }] = useQuery({
    query: TenantSettingsDocument,
    variables: {
      tenantId,
    },
  });
  const [, logIn] = useAtom(starletTokenAtom);
  const [{ auth }, setSettings] = useAtom(starletSettingsAtom);

  useEffect(() => {
    setSettings(settingsQuery?.tenantSetting?.settings || '{}');
  }, [setSettings, settingsQuery]);

  useEffect(() => {
    logIn(auth?.login, auth?.password);
  }, [auth?.login, auth?.password, logIn]);

  return <StarletImporter />;
}
