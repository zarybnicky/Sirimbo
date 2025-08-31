import { Layout } from '@/components/layout/Layout';
import { TenantSettingsDocument, UpdateTenantSettingsDocument } from '@/graphql/CurrentUser';
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
  const settings = JSON.parse(settingsQuery?.tenantSetting?.settings || '{}');
  const loginDetails = useMemo(() => settings?.['evidenceAuth'], [settings]);

  const [loginToken, setLoginToken] = useState<LoginToken | null>(null);
  useEffect(() => {
    if (!loginDetails || typeof loginDetails !== 'object') {
      setLoginToken({ auth_ok: false });
      return;
    }

    fetch('https://evidence.tsstarlet.com/spa_auth/login', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json; charset=utf-8',
      },
      body: loginDetails
    }).then(x => x.json()).then(setLoginToken);
  }, [loginDetails])

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

        <h2>2. Sezóny a složky</h2>
      </div>

      <div className="grid grid-cols-2">
        Složky

        Sezóny

        Uložit výběr
      </div>

      Kurzy

      Uložit výběr


      Studenti

      (De)duplikovaní

      K vytvoření

      Shodní

      Provést import
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
  const create = useMutation(UpdateTenantSettingsDocument)[1];

  useEffect(() => reset(settings['evidenceAuth']), []);

  const onSubmit = useAsyncCallback(async (values: TypeOf<typeof Form>) => {
    await create({
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
