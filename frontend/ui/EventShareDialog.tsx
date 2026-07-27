'use client';

import {
  SetEventSharingDocument,
  type EventWithTrainerFragment,
} from '@/graphql/Event';
import { buttonCls } from '@/ui/style';
import { DialogDescription, DialogTitle } from '@/ui/dialog';
import { Copy, Link2Off } from 'lucide-react';
import React from 'react';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';

export default function EventShareDialog({
  item,
}: { item: EventWithTrainerFragment }) {
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
          <div className="rounded-md border border-neutral-5 bg-neutral-2 p-3">
            <p className="break-all font-mono text-sm text-neutral-12">{path}</p>
          </div>
          <div className="flex flex-wrap gap-2">
            <button
              type="button"
              className={buttonCls({ variant: 'primary' })}
              onClick={async () => {
                await navigator.clipboard.writeText(`${window.location.origin}${path}`);
                toast.success('Odkaz zkopírován.');
              }}
            >
              <Copy />
              Kopírovat odkaz
            </button>
            <button
              type="button"
              className={buttonCls({ variant: 'outline' })}
              disabled={fetching}
              onClick={() => changeSharing(false)}
            >
              <Link2Off />
              Zrušit sdílení
            </button>
          </div>
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
