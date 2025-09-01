import { Layout } from '@/components/layout/Layout';
import { TenantSettingsDocument } from '@/graphql/CurrentUser';
import { TitleBar } from '@/ui/TitleBar';
import React, { useEffect } from 'react';
import { useQuery } from 'urql';
import { starletSettingsAtom, starletTokenAtom } from '@/ui/starlet-importer/state';
import { useAtom } from 'jotai';
import { StarletImporter } from '@/ui/starlet-importer';

export default function StarletImportPage() {
  const [{ data: settingsQuery }] = useQuery({
    query: TenantSettingsDocument,
    variables: {
      tenantId: process.env.NEXT_PUBLIC_TENANT_ID || '1',
    },
  });
  const [, logIn] = useAtom(starletTokenAtom);
  const [{ auth }, setSettings] = useAtom(starletSettingsAtom);
  useEffect(() => {
    setSettings(settingsQuery?.tenantSetting?.settings || '{}')
  }, [setSettings, settingsQuery]);

  useEffect(() => {
    logIn(auth?.login, auth?.password);
  }, [auth?.login, auth?.password, logIn]);

  return (
    <Layout requireAdmin>
      <TitleBar title="Nastavení importu" />

      <StarletImporter />
    </Layout>
  );
};
