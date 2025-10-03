import { useWebPushRegistration } from '@/lib/push-notifications/use-web-push-registration';
import classNames from 'classnames';
import { useMemo } from 'react';

const dateFormatter = new Intl.DateTimeFormat('cs-CZ', {
  dateStyle: 'medium',
  timeStyle: 'short',
});

const formatTimestamp = (value: string | null) => {
  if (!value) return 'Nikdy';
  try {
    return dateFormatter.format(new Date(value));
  } catch {
    return value;
  }
};

const truncate = (value: string, maxLength = 42) =>
  value.length > maxLength ? `${value.slice(0, maxLength)}…` : value;

const channelDescription = (channel: Record<string, unknown> | null | undefined) => {
  if (!channel) return 'Neznámé zařízení';
  const userAgent = typeof channel.userAgent === 'string' ? channel.userAgent : null;
  if (userAgent) return userAgent;
  return 'Webový klient';
};

const statusText: Record<ReturnType<typeof useWebPushRegistration>['status'], string> = {
  checking: 'Ověřujeme stav registrace…',
  'missing-config': 'Chybí veřejný VAPID klíč, kontaktujte administrátora.',
  unsupported: 'Tento prohlížeč nepodporuje web push notifikace.',
  'permission-blocked':
    'Notifikace jsou zablokovány v nastavení prohlížeče. Upravte oprávnění a zkuste to znovu.',
  'requires-auth': 'Pro aktivaci webových notifikací se musíte přihlásit.',
  ready: 'Notifikace nejsou na tomto zařízení aktivní.',
  registering: 'Aktivujeme webové notifikace…',
  registered: 'Webové notifikace jsou aktivní.',
  error: 'Aktivace webových notifikací selhala.',
};

export function PushNotificationsCard() {
  const {
    status,
    error,
    channels,
    activeChannelId,
    register,
    unregister,
    removeChannel,
    isSupported,
  } = useWebPushRegistration();

  const sortedChannels = useMemo(
    () =>
      [...channels].sort((a, b) => {
        if (a.id === activeChannelId) return -1;
        if (b.id === activeChannelId) return 1;
        return (b.lastRegisteredAt || '').localeCompare(a.lastRegisteredAt || '');
      }),
    [channels, activeChannelId],
  );

  const canRegister = isSupported && (status === 'ready' || status === 'error');
  const canUnregister = status === 'registered';

  return (
    <section className="space-y-4 rounded-lg border border-neutral-6 bg-neutral-1 p-6 shadow-sm">
      <header className="space-y-1">
        <h2 className="text-lg font-semibold text-neutral-12">
          Webové notifikace
        </h2>
        <p className="text-sm text-neutral-11">
          Přihlaste svůj prohlížeč k odběru push notifikací z Rozpisovníku. Registrace je
          vázaná na aktuálně přihlášeného uživatele a tenant.
        </p>
      </header>

      <div className="rounded-md border border-neutral-5 bg-neutral-2 p-3 text-sm text-neutral-12">
        {statusText[status]}
        {error && status === 'error' ? (
          <span className="mt-1 block text-xs text-destructive-11">{error}</span>
        ) : null}
      </div>

      <div className="flex flex-wrap gap-3">
        <button
          type="button"
          onClick={register}
          disabled={!canRegister}
          className={classNames(
            'inline-flex items-center justify-center rounded-md border px-4 py-2 text-sm font-medium transition focus:outline-none focus-visible:ring-2 focus-visible:ring-accent-9 focus-visible:ring-offset-2',
            canRegister
              ? 'border-accent-7 bg-accent-9 text-accent-1 hover:bg-accent-10'
              : 'border-neutral-6 bg-neutral-3 text-neutral-11 cursor-not-allowed',
          )}
        >
          Aktivovat v tomto prohlížeči
        </button>
        <button
          type="button"
          onClick={unregister}
          disabled={!canUnregister}
          className={classNames(
            'inline-flex items-center justify-center rounded-md border px-4 py-2 text-sm font-medium transition focus:outline-none focus-visible:ring-2 focus-visible:ring-neutral-10 focus-visible:ring-offset-2',
            canUnregister
              ? 'border-neutral-7 bg-neutral-1 text-neutral-12 hover:bg-neutral-3'
              : 'border-neutral-6 bg-neutral-3 text-neutral-11 cursor-not-allowed',
          )}
        >
          Odhlásit tento prohlížeč
        </button>
      </div>

      <div className="space-y-2">
        <h3 className="text-sm font-semibold text-neutral-12">Aktivní kanály</h3>
        {sortedChannels.length === 0 ? (
          <p className="text-sm text-neutral-11">
            Žádný aktivní kanál zatím neexistuje. Po aktivaci se zde zobrazí registrovaná
            zařízení.
          </p>
        ) : (
          <ul className="space-y-2">
            {sortedChannels.map((channel) => {
              const isCurrent = channel.id === activeChannelId;
              const credentials = (channel.credentials as Record<string, unknown> | null) ?? null;
              const endpointLabel = truncate(channel.channelIdentifier, 64);

              return (
                <li
                  key={channel.id}
                  className={classNames(
                    'flex flex-col gap-2 rounded-md border p-3 text-sm md:flex-row md:items-center md:justify-between',
                    isCurrent
                      ? 'border-accent-7 bg-accent-3/30'
                      : 'border-neutral-5 bg-neutral-1',
                  )}
                >
                  <div>
                    <p className="font-medium text-neutral-12">
                      {isCurrent ? 'Tento prohlížeč' : channelDescription(credentials)}
                    </p>
                    <p className="text-xs text-neutral-11">
                      Poslední registrace: {formatTimestamp(channel.lastRegisteredAt)}
                    </p>
                    <p className="text-xs text-neutral-10">{endpointLabel}</p>
                  </div>
                  <div className="flex items-center gap-2 self-start md:self-center">
                    <span className="rounded-full bg-accent-4 px-2 py-0.5 text-xs font-medium text-accent-11">
                      {channel.provider}
                    </span>
                    {!isCurrent ? (
                      <button
                        type="button"
                        onClick={() => removeChannel(channel.id)}
                        className="text-xs font-medium text-destructive-11 hover:underline"
                      >
                        Odebrat
                      </button>
                    ) : null}
                  </div>
                </li>
              );
            })}
          </ul>
        )}
      </div>
    </section>
  );
}
