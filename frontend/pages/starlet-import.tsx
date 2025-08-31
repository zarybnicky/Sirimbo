import { Layout } from '@/components/layout/Layout';
import { EvidenceStarletDocument, TenantSettingsDocument, UpdateTenantSettingsDocument } from '@/graphql/CurrentUser';
import { TitleBar } from '@/ui/TitleBar';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import React, { useEffect, useMemo, useState } from 'react';
import { useQuery } from 'urql';
import { UserRole } from '@/starlet/graphql';
import { useZodForm } from '@/lib/use-schema-form';
import { TextFieldElement } from '@/ui/fields/text';
import { useFormResult } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation } from 'urql';
import { type TypeOf, z } from 'zod';
import { fetchGql } from '@/graphql/query';
import { FoldersAndSeasonsDocument } from '@/starlet/graphql/Query';
import { print } from '@0no-co/graphql.web';
import { Checkbox } from '@/ui/fields/checkbox';

type LoginToken =
  | { auth_ok: false }
  | {
    auth_ok: true;
    login: string;
    auth_token: string;
    role: UserRole;
    name: string;
    cgroup_key: string;
  };

export default function ProfilePage() {
  const tenantId = process.env.NEXT_PUBLIC_TENANT_ID || '1';
  const [{ data: settingsQuery }] = useQuery({
    query: TenantSettingsDocument,
    variables: { tenantId },
  });
  const update = useMutation(UpdateTenantSettingsDocument)[1];

  const settings = JSON.parse(settingsQuery?.tenantSetting?.settings || '{}');
  const loginDetails = useMemo(() => settings?.['evidenceAuth'], [JSON.stringify(settings)]);

  const [loginToken, setLoginToken] = useState<LoginToken | null>(null);
  useEffect(() => {
    console.log('tying to log in')
    if (loginToken?.auth_ok)
      return;
    if (!loginDetails?.login || !loginDetails?.password) {
      console.log('skipping login')
      setLoginToken({ auth_ok: false });
      return;
    }

    console.log('logging in')
    fetchGql(EvidenceStarletDocument, {
      url: 'https://evidence.tsstarlet.com/spa_auth/login',
      data: JSON.stringify({
        login: loginDetails.login,
        password: loginDetails.password,
      }),
    }).then(x => setLoginToken(JSON.parse(x.evidenceStarlet)));
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [loginDetails?.login, loginDetails?.password]);

  const [foldersAndSeasons, setFoldersAndSeasons] = useState<{
    data: {
      folders: {
        key: string;
        name: string;
        order_value: number;
      }[];
      seasons: {
        key: string;
        name: string;
        order_value: number;
      }[];
    }
  } | null>(null);

  useEffect(() => {
    if (!loginToken?.auth_ok) return

    fetchGql(EvidenceStarletDocument, {
      url: 'https://evidence.tsstarlet.com/graphql',
      data: JSON.stringify({
        query: print(FoldersAndSeasonsDocument),
        variables: {},
      }),
      auth: loginToken.auth_token,
    }).then(x => setFoldersAndSeasons(JSON.parse(x.evidenceStarlet)));
  }, [loginToken]);

  const [selectedFolders, setSelectedFolders] = useState(new Set());
  const [selectedSeasons, setSelectedSeasons] = useState(new Set());
  useEffect(() => {
    console.log('updating folders and seasons');
    setSelectedFolders(new Set(settings?.['evidenceFolders']));
    setSelectedSeasons(new Set(settings?.['evidenceSeasons']));
  }, [JSON.stringify(settings)]);

  const onSubmitFolders = useAsyncCallback(async () => {
    await update({
      input: {
        tenantId,
        patch: {
          settings: JSON.stringify({ ...settings, evidenceFolders: [...selectedFolders.values()], evidenceSeasons: [...selectedSeasons.values()] })
        }
      },
    });
  });

  return (
    <Layout requireAdmin>
      <TitleBar title="Můj profil" />

      <div className="prose">
        <h2>1. Přihlašovací údaje</h2>

        <p>
          Přihlašovací údaje {loginDetails ? 'vyplněny' : 'nevyplněny'}
          <Dialog>
            <DialogTrigger size="sm" text="Změnit přihlašovací údaje" />
            <DialogContent>
              <ChangeLoginForm tenantId={tenantId} settings={settings} />
            </DialogContent>
          </Dialog>
        </p>

        <p>
          {loginToken ?
            (loginToken.auth_ok ?
              `Přihlášen jako ${loginToken.login}` :
              'Neplatné přihlašovací údaje') :
            'Pokouším se přihlásit...'}
        </p>

        <h2 className="mb-0">2. Sezóny a složky</h2>
        <div className="grid grid-cols-2">
          {foldersAndSeasons ? (
            <>
              <ul>
                {foldersAndSeasons.data.folders
                  .sort((x, y) => x.order_value - y.order_value)
                  .map(x => (
                    <li key={x.key}>
                    <Checkbox name={x.key} label={x.name} value={x.key} checked={selectedFolders.has(x.key)} onChange={(e) => setSelectedFolders(fs => (e.target as any).checked ? fs.union(new Set([x.key])) : fs.difference(new Set([x.key])))} />
                    </li>
                  ))
                }
              </ul>
              <ul>
                {foldersAndSeasons.data.seasons
                  .sort((x, y) => x.order_value - y.order_value)
                  .map(x => (
                    <li key={x.key}>
                      <Checkbox name={x.key} label={x.name} value={x.key} checked={selectedSeasons.has(x.key)} onChange={(e) => setSelectedSeasons(fs => (e.target as any).checked ? fs.union(new Set([x.key])) : fs.difference(new Set([x.key])))} />
                    </li>
                  ))
                }
              </ul>
            </>
          ) : null}
        </div>

        <SubmitButton type="button" loading={onSubmitFolders.loading} onClick={onSubmitFolders.execute} />

        <h2>3. Kurzy</h2>

        Uložit výběr

        <h2>4. Studenti</h2>
        (De)duplikovaní

        K vytvoření

        Shodní

        Provést import
      </div>
    </Layout>
  );
};



const Form = z.object({
  login: z.string(),
  password: z.string(),
});

export function ChangeLoginForm({ tenantId, settings }: {
  tenantId: string;
  settings: Record<string, object>;
}) {
  const { onSuccess } = useFormResult();
  const { control, handleSubmit, reset } = useZodForm(Form);
  const update = useMutation(UpdateTenantSettingsDocument)[1];

  useEffect(() => reset(settings['evidenceAuth']), []);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    await update({
      input: {
        tenantId,
        patch: {
          settings: JSON.stringify({ ...settings, evidenceAuth: values })
        }
      },
    });
    onSuccess();
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TextFieldElement control={control} name="login" label="Přihlašovací jméno" />

      <TextFieldElement control={control} name="password" label="Heslo" />

      Otestovat přihlášení

      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}
