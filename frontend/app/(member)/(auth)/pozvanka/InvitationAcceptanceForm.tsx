'use client';

import { acceptInvitationAction } from '@/lib/auth-actions';
import { TextField } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { cardCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { useAsyncCallback } from 'react-async-hook';

type Props = {
  token: string;
  name: string;
  accountEmail: string;
};

export function InvitationAcceptanceForm({ token, name, accountEmail }: Props) {
  const accept = useAsyncCallback(async () => {
    const result = await acceptInvitationAction({ token });
    if ('error' in result) throw new Error(result.error);
    window.location.assign(result.redirectTo);
  });

  return (
    <div className={cardCls()}>
      <div className="grid gap-2 p-4">
        <h4 className="text-2xl">Přijmout pozvánku</h4>
        <FormError error={accept.error} />
        <p>Pozvánku přijmete pod účtem uvedeným níže.</p>
        <TextField name="name" label="Osoba" value={name} readOnly />
        <TextField
          name="accountEmail"
          label="Přihlášený účet"
          value={accountEmail}
          readOnly
        />
        <SubmitButton action={accept} className="w-full my-2">
          Přijmout pozvánku
        </SubmitButton>
      </div>
    </div>
  );
}
