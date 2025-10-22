import { Layout } from '@/ui/Layout';
import { TenantSettingsDocument } from '@/graphql/CurrentUser';
import { tenantConfig } from '@/tenant/config';
import { TitleBar } from '@/ui/TitleBar';
import React, { useEffect } from 'react';
import { useQuery } from 'urql';
import { starletSettingsAtom, starletTokenAtom } from '@/ui/starlet-importer/state';
import { useAtom, useAtomValue } from 'jotai';
import { StarletImporter } from '@/ui/starlet-importer';
import { useRouter } from 'next/router';
import { tenantIdAtom } from '@/ui/state/auth';

export default function StarletImportPage() {
  const router = useRouter();
  const tenantId = useAtomValue(tenantIdAtom);
  const isEnabled = Boolean(tenantConfig.enableStarletImport);
  const [{ data: settingsQuery }] = useQuery({
    query: TenantSettingsDocument,
    pause: !isEnabled,
    variables: {
      tenantId,
    },
  });
  const [, logIn] = useAtom(starletTokenAtom);
  const [{ auth }, setSettings] = useAtom(starletSettingsAtom);

  useEffect(() => {
    if (!isEnabled) {
      void router.replace('/404');
    }
  }, [isEnabled, router]);

  useEffect(() => {
    if (!isEnabled) return;
    setSettings(settingsQuery?.tenantSetting?.settings || '{}')
  }, [isEnabled, setSettings, settingsQuery]);

  useEffect(() => {
    if (!isEnabled) return;
    logIn(auth?.login, auth?.password);
  }, [auth?.login, auth?.password, isEnabled, logIn]);

  if (!isEnabled) {
    return null;
  }

  return (
    <Layout requireAdmin>
      <TitleBar title="Nastavení importu" />

      <StarletImporter />
    </Layout>
  );
};
