import { Layout } from '@/ui/Layout';
import { TenantSettingsDocument } from '@/graphql/CurrentUser';
import { TitleBar } from '@/ui/TitleBar';
import React, { useEffect } from 'react';
import { useQuery } from 'urql';
import { starletSettingsAtom, starletTokenAtom } from '@/ui/starlet-importer/state';
import { useAtom, useAtomValue } from 'jotai';
import { StarletImporter } from '@/ui/starlet-importer';
import { useRouter } from 'next/router';
import { tenantConfigAtom, tenantIdAtom } from '@/ui/state/auth';

export default function StarletImportPage() {
  const router = useRouter();
  const tenantId = useAtomValue(tenantIdAtom);
  const { enableStarletImport } = useAtomValue(tenantConfigAtom);
  const [{ data: settingsQuery }] = useQuery({
    query: TenantSettingsDocument,
    pause: !enableStarletImport,
    variables: {
      tenantId,
    },
  });
  const [, logIn] = useAtom(starletTokenAtom);
  const [{ auth }, setSettings] = useAtom(starletSettingsAtom);

  useEffect(() => {
    if (!enableStarletImport) {
      void router.replace('/404');
    }
  }, [enableStarletImport, router]);

  useEffect(() => {
    if (!enableStarletImport) return;
    setSettings(settingsQuery?.tenantSetting?.settings || '{}')
  }, [enableStarletImport, setSettings, settingsQuery]);

  useEffect(() => {
    if (!enableStarletImport) return;
    logIn(auth?.login, auth?.password);
  }, [auth?.login, auth?.password, enableStarletImport, logIn]);

  if (!enableStarletImport) {
    return null;
  }

  return (
    <Layout requireAdmin>
      <TitleBar title="Nastavení importu" />

      <StarletImporter />
    </Layout>
  );
};
