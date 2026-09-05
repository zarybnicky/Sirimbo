'use client';

import { SetEventSharingDocument, type EventWithTrainerFragment } from '@/graphql/Event';
import { InputGroup } from '@/ui/fields/text';
import { buttonCls, inputCls } from '@/ui/style';
import { DialogDescription, DialogTitle } from '@/ui/dialog';
import { Copy, Link2Off } from 'lucide-react';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';

export default function EventShareDialog({ item }: { item: EventWithTrainerFragment }) {
  const [{ fetching }, setSharing] = useMutation(SetEventSharingDocument);
  const [token, setToken] = React.useState(item.shareToken);

  const changeSharing = async (enabled: boolean) => {
    const result = await setSharing({
      input: {
        id: item.id,
        pEnabled: enabled,
      },
    });
    if (!result.error) {
      setToken(result.data?.setEventSharing?.string ?? null);
    }
  };

  const path = token ? `/termin/${item.id}?share=${token}` : null;

  return (
    <div className="space-y-4">
      <div>
        <DialogTitle>Odkaz ke sdílení</DialogTitle>
        <DialogDescription className="mt-2">
          Použitím odkazu se kdokoli může podívat na rozpis události.
        </DialogDescription>
      </div>

      {path ? (
        <>
          <InputGroup>
            <input
              readOnly
              value={path}
              aria-label="Odkaz ke sdílení"
              className={inputCls({ className: 'min-w-0 grow font-mono' })}
            />
            <button
              type="button"
              className={buttonCls({
                variant: 'primary',
                className: 'shrink-0',
              })}
              onClick={async () => {
                await navigator.clipboard.writeText(`${window.location.origin}${path}`);
                toast.success('Odkaz zkopírován.');
              }}
              title="Kopírovat odkaz"
            >
              <Copy />
            </button>
          </InputGroup>
          <button
            type="button"
            className={buttonCls({ variant: 'outline' })}
            disabled={fetching}
            onClick={() => changeSharing(false)}
          >
            <Link2Off />
            Zrušit sdílení
          </button>
        </>
      ) : (
        <button
          type="button"
          className={buttonCls({ variant: 'primary' })}
          disabled={fetching}
          onClick={() => changeSharing(true)}
        >
          Vytvořit odkaz ke sdílení
        </button>
      )}
    </div>
  );
}
